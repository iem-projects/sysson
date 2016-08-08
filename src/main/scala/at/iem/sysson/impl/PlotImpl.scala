/*
 *  PlotImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package impl

import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.{IntObj, StringObj}
import de.sciss.lucre.matrix.{Dimension, Matrix, Reduce}
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Elem, Copy, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.annotation.tailrec

object PlotImpl {
  private def findDim[S <: Sys[S]](matrix: Matrix[S], names: List[String])(implicit tx: S#Tx): Option[Matrix[S]] =
    matrix.dimensions.find { d =>
      val n = d.name.toLowerCase
      names.contains(n)
    }

  private def tryAddDim[S <: Sys[S]](matrix: Matrix[S],
                                     dims: evt.Map.Modifiable[S, String, StringObj],
                                     key: String, names: List[String])(implicit tx: S#Tx): Unit =
    findDim(matrix, names).foreach(dim => dims.put(key, StringObj.newVar(StringObj.newConst[S](dim.name))))

  @tailrec private def dropVar[S <: Sys[S]](m: Matrix[S])(implicit tx: S#Tx): Matrix[S] = m match {
    case Matrix.Var(vr) => dropVar(vr())
    case _ => m
  }

  private def mkVar[S <: Sys[S]](m: Matrix[S])(implicit tx: S#Tx): Matrix[S] = m match {
    case Matrix.Var(vr) => vr
    case _ => Matrix.Var(m)
  }

  def apply[S <: Sys[S]](matrix: Matrix[S])(implicit tx: S#Tx): Plot[S] = {
    val targets = evt.Targets[S]
    val dims    = evt.Map.Modifiable[S, String, StringObj]
    tryAddDim(matrix, dims, Plot.VKey, "lat" :: "latitude"  :: Nil)
    tryAddDim(matrix, dims, Plot.HKey, "lon" :: "longitude" :: Nil)
    val m1 = if (matrix.reducedRank <= 2) matrix else
      findDim(matrix, "time" :: Nil).fold(matrix) { dimTime =>
        Reduce(dropVar(matrix), Dimension.Selection.Name(StringObj.newConst[S](dimTime.name)),
          Reduce.Op.Apply(IntObj.newVar(IntObj.newConst[S](0))))
      }

    val m2 = mkVar(m1)
    new Impl(targets, m2, dims).connect()
  }

  /** NOTE: does not copy the arguments but assumes they have been! */
  def copy[S <: Sys[S]](matrix: Matrix[S], dims: evt.Map.Modifiable[S, String, StringObj])
                       (implicit tx: S#Tx): Plot[S] = {
    val targets = evt.Targets[S]
    new Impl(targets, matrix, dims).connect()
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Plot[S] =
    serializer[S].read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Plot[S]] = anySer.asInstanceOf[Ser[S]]

  private final val SER_VERSION = 0x506C7400  // "Plt\0"

  private val anySer = new Ser[NoSys]

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Plot[S] = {
    val targets   = Targets.read(in, access)
    val cookie    = in.readInt()
    if (cookie != SER_VERSION)
      sys.error(s"Unexpected cookie (expected ${SER_VERSION.toHexString}, found ${cookie.toHexString})")

    val matrix    = Matrix.read(in, access)
    val dims      = evt.Map.read[S, String, StringObj](in, access)
    new Impl(targets, matrix, dims)
  }

  private final class Ser[S <: Sys[S]] extends ObjSerializer[S, Plot[S]] {
    def tpe: Obj.Type = Plot
  }

  // XXX TODO: listen to matrix
  private final class Impl[S <: Sys[S]](protected val targets: Targets[S],
                                        val matrix: Matrix[S],
                                        val dims: evt.Map[S, String, StringObj])
    extends Plot[S]
    with evt.impl.SingleNode[S, Plot.Update[S]] {
    source =>

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val targetsOut  = Targets[Out]
      val matrixOut   = context(matrix)
      val dimsOut     = evt.Map.Modifiable[Out, String, StringObj]
//      context.defer(source, res) {
//        import StringObj.{serializer => stringSer, varSerializer => stringVarSer}
//        dims.iterator.foreach { case (key, value) =>
//          val valueCpy = value match {
//            case StringObj.Var(vr) => StringObj.newVar(vr())
//            case other => other
//          }
//          dimsCpy.put(key, valueCpy)
//        }
//      }
      val res = new Impl[Out](targetsOut, matrixOut, dimsOut).connect()
      context.defer(source, res) {
        dims.iterator.foreach { case (k, v) =>
          dimsOut.put(k, context(v))
        }
      }
      res
    }

    def connect   ()(implicit tx: S#Tx): this.type = {
      matrix.changed ---> changed
      dims  .changed ---> changed
      this
    }

    private def disconnect()(implicit tx: S#Tx): Unit = {
      matrix.changed -/-> changed
      dims  .changed -/-> changed
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      disconnect()
      dims.dispose()
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(SER_VERSION)
      matrix.write(out)
      dims  .write(out)
    }

    // def changed: EventLike[S, Plot.Update[S]] = this

    // protected def reader: evt.Reader[S, Plot[S]] = PlotImpl.serializer

    object changed extends Changed {
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Plot.Update[S]] = {
        val mch = matrix.changed
        var ch = Vector.empty[Plot.Change[S]]
        if (pull.contains(mch)) pull(mch).foreach(u => ch :+= Plot.MatrixChange(u))
        val dch = dims.changed
        if (pull.contains(dch)) pull(dch).foreach(u => ch :+= Plot.DimsChange  (u))
        if (ch.isEmpty) None else Some(Plot.Update(source, ch))
      }
    }
  }

  // ---- elem ----

  //  // println("register sonification")
  //  private lazy val _init: Unit = Elem.registerExtension(ElemImpl)
  //
  //  def init(): Unit = _init

//  def mkCopy()(implicit tx: S#Tx): Plot.Elem[S] = {
//    val matrixCpy = peer.matrix.mkCopy()
//    import StringObj.{serializer => stringSer, varSerializer => stringVarSer}
//    val dimsCpy = expr.Map.Modifiable[S, String, Expr[S, String], model.Change[String]]
//    peer.dims.iterator.foreach {
//      case (key, StringObj.Var(vr))  => dimsCpy.put(key, StringObj.newVar(vr()))
//      case (key, value)         => dimsCpy.put(key, value)
//    }
//    val plotCpy = PlotImpl.copy(matrixCpy, dimsCpy)
//    Plot.Elem(plotCpy)
//  }
}

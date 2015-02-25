/*
 *  PlotImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package impl

import de.sciss.lucre.expr.{Expr, String => StringEx}
import de.sciss.lucre.matrix.Matrix
import de.sciss.lucre.{event => evt, expr}
import de.sciss.lucre.event.{EventLike, Sys}
import de.sciss.model
import de.sciss.serial.{DataOutput, DataInput}
import de.sciss.synth.proc.Elem
import de.sciss.synth.proc.impl.{ActiveElemImpl, ElemCompanionImpl}

object PlotImpl {
  def apply[S <: Sys[S]](matrix: Matrix[S])(implicit tx: S#Tx): Plot[S] = {
    val targets = evt.Targets[S]
    import StringEx.{serializer => stringSer, varSerializer => stringVarSer}
    val dims    = expr.Map.Modifiable[S, String, Expr[S, String], model.Change[String]]
    new Impl(targets, matrix, dims)
  }

  /** NOTE: does not copy the arguments but assumes they have been! */
  def copy[S <: Sys[S]](matrix: Matrix[S], dims: expr.Map.Modifiable[S, String, Expr[S, String], model.Change[String]])
                       (implicit tx: S#Tx): Plot[S] = {
    val targets = evt.Targets[S]
    new Impl(targets, matrix, dims)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Plot[S] =
    serializer[S].read(in, access)

  implicit def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Plot[S]] = anySer.asInstanceOf[Ser[S]]

  private final val SER_VERSION = 0x506C7400  // "Plt\0"

  private val anySer = new Ser[evt.InMemory]

  private final class Ser[S <: Sys[S]] extends evt.NodeSerializer[S, Plot[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Plot[S] = {
      implicit val str = StringEx.serializer[S]
      val cookie = in.readInt()
      require(cookie == SER_VERSION,
        s"Unexpected cookie (expected ${SER_VERSION.toHexString}, found ${cookie.toHexString})")
      val matrix    = Matrix.read(in, access)
      val dims      = expr.Map.read[S, String, Expr[S, String], model.Change[String]](in, access)
      new Impl(targets, matrix, dims)
    }
  }

  // XXX TODO: listen to matrix
  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                        val matrix: Matrix[S],
                                        val dims: expr.Map[S, String, Expr[S, String], model.Change[String]])
    extends Plot[S]
    with evt.impl.StandaloneLike[S, Plot.Update[S], Plot[S]] {
    source =>

    def mkCopy()(implicit tx: S#Tx): Plot[S] = {
      val tgt       = evt.Targets[S]
      val matrixCpy = matrix.mkCopy()
      import StringEx.{varSerializer => stringVarSer, serializer => stringSer}
      val dimsCpy   = expr.Map.Modifiable[S, String, Expr[S, String], model.Change[String]]
      dims.iterator.foreach { case (key, value) =>
        val valueCpy = value match {
          case Expr.Var(vr) => StringEx.newVar(vr())
          case other => other
        }
        dimsCpy.put(key, valueCpy)
      }
      new Impl[S](tgt, matrixCpy, dimsCpy)
    }

    def disconnect()(implicit tx: S#Tx): Unit = {
      matrix.changed ---> this
      dims  .changed ---> this
    }

    def connect   ()(implicit tx: S#Tx): Unit = {
      matrix.changed -/-> this
      dims  .changed -/-> this
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = dims.dispose()

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(SER_VERSION)
      matrix.write(out)
      dims.write(out)
    }

    def changed: EventLike[S, Plot.Update[S]] = this

    protected def reader: evt.Reader[S, Plot[S]] = PlotImpl.serializer

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Plot.Update[S]] = {
      val mch = matrix.changed
      var ch = Vector.empty[Plot.Change[S]]
      if (pull.contains(mch)) pull(mch).foreach(u => ch :+= Plot.MatrixChange(u))
      val dch = dims.changed
      if (pull.contains(dch)) pull(dch).foreach(u => ch :+= Plot.DimsChange  (u))
      if (ch.isEmpty) None else Some(Plot.Update(source, ch))
    }
  }

  // ---- elem ----

  // println("register sonification")
  private lazy val _init: Unit = Elem.registerExtension(ElemImpl)

  def init(): Unit = _init

  object ElemImpl extends ElemCompanionImpl[Plot.Elem] {
    final val typeID = Plot.typeID

    def apply[S <: Sys[S]](peer: Plot[S])(implicit tx: S#Tx): Plot.Elem[S] = {
      val targets = evt.Targets[S]
      new Impl[S](targets, peer)
    }

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Plot.Elem[S] =
      serializer[S].read(in, access)

    // ---- Elem.Extension ----

    /** Read identified active element */
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                   (implicit tx: S#Tx): Plot.Elem[S] with evt.Node[S] = {
      val peer = Plot.read(in, access)
      new Impl[S](targets, peer)
    }

    /** Read identified constant element */
    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Plot.Elem[S] =
      sys.error("Constant Plot not supported")

    // ---- implementation ----

    private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                          val peer: Plot[S])
      extends Plot.Elem[S]
      with ActiveElemImpl[S] {

      def typeID = ElemImpl.typeID
      def prefix = "Plot"

      override def toString() = s"$prefix$id"

      def mkCopy()(implicit tx: S#Tx): Plot.Elem[S] = {
        val matrixCpy = peer.matrix.mkCopy()
        import StringEx.{serializer => stringSer, varSerializer => stringVarSer}
        val dimsCpy = expr.Map.Modifiable[S, String, Expr[S, String], model.Change[String]]
        peer.dims.iterator.foreach {
          case (key, Expr.Var(vr))  => dimsCpy.put(key, StringEx.newVar(vr()))
          case (key, value)         => dimsCpy.put(key, value)
        }
        val plotCpy = PlotImpl.copy(matrixCpy, dimsCpy)
        Plot.Elem(plotCpy)
      }
    }
  }
}

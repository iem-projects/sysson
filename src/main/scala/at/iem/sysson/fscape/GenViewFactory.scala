/*
 *  GenViewFactory.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package fscape

import at.iem.sysson.fscape.graph.{Dim, Matrix, UserValue}
import at.iem.sysson.sound.AuralSonification
import de.sciss.equal.Implicits._
import de.sciss.fscape.lucre.FScape.{Output, Rendering}
import de.sciss.fscape.lucre.UGenGraphBuilder.{IO, Input, MissingIn}
import de.sciss.fscape.lucre.impl.{RenderingImpl, UGenGraphBuilderContextImpl}
import de.sciss.fscape.lucre.{FScape, OutputGenView, UGenGraphBuilder}
import de.sciss.fscape.stream.Control
import de.sciss.lucre.matrix.{DataSource, Matrix => LMatrix}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.{GenContext, GenView, WorkspaceHandle}

object GenViewFactory {
  def apply  (config: Control.Config = Control.Config()): GenView.Factory = new Impl(config)
  def install(config: Control.Config = Control.Config()): Unit            = GenView.addFactory(apply(config))

  def render[S <: Sys[S]](fscape: FScape[S], config: Control.Config = Control.Config())
                         (implicit tx: S#Tx, context: GenContext[S]): FScape.Rendering[S] = {
    val ugbCtx: UGenGraphBuilder.Context[S] = ??? // RRR new ContextImpl(fscape)
    RenderingImpl(fscape, ugbCtx, config, force = true)
  }
/*
  private final class ContextImpl[S <: Sys[S]](protected val fscape: FScape[S])(implicit context: GenContext[S])
    extends UGenGraphBuilderContextImpl[S] {

    implicit protected def cursor    : stm.Cursor[S]       = context.cursor
    implicit protected def workspace : WorkspaceHandle[S]  = context.workspaceHandle

    private def findMatrix(vr: Matrix)(implicit tx: S#Tx): LMatrix[S] = {
      val f       = fscape
      val vrName  = vr.name
      val mOpt0   = f.attr.$[LMatrix](vrName)
      val mOpt    = mOpt0.orElse {
        for {
          sonif  <- AuralSonification.find[S]()
          source <- sonif.sources.get(vrName)
        } yield source.matrix
      }
      val m     = mOpt.getOrElse(throw MissingIn(vrName))
      m
    }

    private def requestDim(dim: Dim)(implicit tx: S#Tx): (LMatrix[S], Int) = {
      val f       = fscape
      val vrName  = dim.variable.name
      val dimNameL= dim.name
      val mOpt0   = f.attr.$[LMatrix](vrName)
      val mOpt    = mOpt0.fold {
        for {
          sonif     <- AuralSonification.find[S]()
          source    <- sonif.sources.get(vrName)
          dimNameEx <- source.dims.get(dimNameL)
          dimName    = dimNameEx.value
          m          = source.matrix
          dimIdx     = m.dimensions.indexWhere(_.name === dimName)
          if dimIdx >= 0
        } yield (m, dimIdx)
      } { _m =>
        val dimIdx = _m.dimensions.indexWhere(_.name === dimNameL)
        if (dimIdx < 0) None else Some((_m, dimIdx))
      }
      mOpt.getOrElse(throw MissingIn(vrName))
    }

    private def requestDimInfo(i: Dim.InfoGE)(implicit tx: S#Tx): Dim.Info = {
      val (m, dimIdx) = requestDim(i.dim)
      val d = m.dimensions.apply(dimIdx)

      val res: Dim.Info = new Dim.Info {
        val variable: Matrix.Info = mkMatrixInfo(m)
        val index   : Int         = dimIdx
        val matrix  : LMatrix.Key = m.getDimensionKey(dimIdx, useChannels = false)
        val shape   : Vec[Int]    = d.shape
        val name    : String      = d.name
        val units   : String      = d.units
      }
      res
    }

    private def requestMatrixInfo(i: Matrix.InfoGE)(implicit tx: S#Tx): Matrix.Info = {
      val m = findMatrix(i.variable)
      mkMatrixInfo(m)
    }

    private def mkMatrixInfo(m: LMatrix[S])(implicit tx: S#Tx): Matrix.Info =
      new Matrix.Info {
        val matrix  : LMatrix.Key = m.getKey(-1)
        val shape   : Vec[Int]    = m.shape
        val name    : String      = m.name
        val units   : String      = m.units
      }

    private def requestVarSpec(i: Matrix.Spec, io: IO[S] with UGenGraphBuilder)
                              (implicit tx: S#Tx): Matrix.Spec.Value = {
      val mRef = i.variable
      val spec0: Matrix.Spec.Value = if (mRef != null) /* XXX TODO --- dirty hack */  {
        val m = findMatrix(mRef)

        implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]

        val dimsIn  = m.dimensions
        val rank    = dimsIn.size // m.rank
        val dims0: Vec[Matrix.Spec.Dim] = Vector.tabulate(rank) { dimIdx =>
          val dimKey  = m.getDimensionKey(dimIdx, useChannels = false)
  //        val reader: LMatrix.Reader = dim.reader(-1)
          val reader: LMatrix.Reader = dimKey.reader()
          val lenL  = reader.size
          require(lenL <= 0x7FFFFFFF)
          val len   = lenL.toInt
          val buf   = new Array[Double](len)
          reader.readDouble1D(buf, 0, len)
          val dim   = dimsIn(dimIdx)
          Matrix.Spec.Dim(dim.name, dim.units, buf.toIndexedSeq)
        }
        Matrix.Spec.Value(name = m.name, units = m.units, dimensions = dims0)
      } else {
        Matrix.Spec.Value(name = "test", units = "", dimensions = Vector.empty)
      }

      def resolveDimIdx(specIn: Matrix.Spec.Value, dimRef: Dim): Int = {
        require(dimRef.variable === i.variable)
        // N.B. Because we may drop multiple times,
        // we have to "relocate" the dimensional index,
        // simply by looking up its name
        val (m0, dimIdx0) = requestDim(dimRef)
        val dimName       = m0.dimensions.apply(dimIdx0).name
        val dimIdx        = specIn.dimensions.indexWhere(_.name === dimName)
        require(dimIdx >= 0)
        dimIdx
      }

      val spec = (spec0 /: i.ops) {
        case (specIn, Matrix.Op.Drop(dimRef)) =>
          val dimIdx = resolveDimIdx(specIn, dimRef)
          specIn.copy(dimensions = specIn.dimensions.patch(dimIdx, Nil, 1))

        case (specIn, Matrix.Op.MoveLast(dimRef)) =>
          val dimIdx = resolveDimIdx(specIn, dimRef)
          val dims0 = specIn.dimensions
          val dim   = dims0(dimIdx)
          specIn.copy(dimensions = dims0.patch(dimIdx, Nil, 1) :+ dim)

        case (specIn, Matrix.Op.Append(dimDef)) =>
          val dims0   = specIn.dimensions
          val valuesC = UGenGraphBuilder.resolveSeq(dimDef.values, io) match {
            case Right(xs) => xs
            case Left(msg) => throw new Exception(msg)
          }
          val values  = valuesC.map(_.doubleValue)
          val dim     = Matrix.Spec.Dim(name = dimDef.name, units = dimDef.units, values = values)
          specIn.copy(dimensions = dims0 :+ dim)
      }

      spec
    }

    private def requestMatrixValueSeq(vr: Matrix)(implicit tx: S#Tx): Matrix.ValueSeq.Value = {
      val m     = findMatrix(vr)
      val res   = new Matrix.ValueSeq.Value {
        val matrix: LMatrix.Key = m.getKey(-1) // reader(-1)

        // XXX TODO: `reader` is called from non-txn
        // and thus we need to create it early here. This is not
        // really cool, because it means the file is opened if
        // if just the cache is validated. We should perhaps change
        // in LucreMatrix the API to use `TxnLike` (what workspace-addDependent uses)
        // instead of `S#Tx`, so we can insert a cheap single txn here
        val /* def */ reader: LMatrix.Reader = {
          implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]
          matrix.reader()
        }
      }
      res
    }

    private def requestMatrixValueWindow(vr: Matrix, dimRefs: Vec[Dim])(implicit tx: S#Tx): Matrix.ValueWindow.Value = {
      val m       = findMatrix(vr)
      val dimsB   = List.newBuilder[Int]
      val shape   = m.shape
      dimsB.sizeHint(dimRefs.size)
      var _winSize = if (dimRefs.isEmpty) 0L else 1L
      dimRefs.foreach { dimRef =>
        val (m1, dimIdx) = requestDim(dimRef)
        require(m1 === m)
        _winSize *= shape(dimIdx)
        dimsB += dimIdx
      }

      val res   = new Matrix.ValueWindow.Value {
        val matrix: LMatrix.Key = m.getKey(-1)
        val reader: LMatrix.Reader = {
          implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]
          matrix.reader()
        }

        val winSize: Long = _winSize

        val dims: List[Int] = dimsB.result()
      }
      res
    }

    private def requestUserValue(req: UserValue)(implicit tx: S#Tx): UserValue.Value = {
      val key    = req.name
      val valOpt = for {
        sonif  <- AuralSonification.find[S]()
        source <- sonif.controls.get(key)
      } yield source.value
      
      UserValue.Value(valOpt)
    }

    override def requestInput[Res](req: Input {type Value = Res}, io: IO[S] with UGenGraphBuilder)
                                  (implicit tx: S#Tx): Res = req match {
      case Matrix.ValueSeq    (vr)       => requestMatrixValueSeq   (vr)
      case Matrix.ValueWindow (vr, dims) => requestMatrixValueWindow(vr, dims)
      case i: Dim.Size                   => requestDimInfo   (i)
      case i: Dim.SuccSize               => requestDimInfo   (i)
      case i: Matrix.Size                => requestMatrixInfo(i)
      case i: Matrix.Rank                => requestMatrixInfo(i)
      case i: Matrix.Spec                => requestVarSpec   (i, io)
      case i: UserValue                  => requestUserValue (i)

      case _ => super.requestInput(req, io)
    }
  }
*/
  private final class Impl(config: Control.Config) extends GenView.Factory {
    def typeID: Int = Output.typeID

    type Repr[~ <: Sys[~]] = Output[~]

    def apply[S <: Sys[S]](output: Output[S])(implicit tx: S#Tx, context: GenContext[S]): GenView[S] = {
      val _fscape = output.fscape
      val fscView = context.acquire[Rendering[S]](_fscape) {
        val ugbCtx: UGenGraphBuilder.Context[S] = ??? // RRR new ContextImpl(_fscape)
        RenderingImpl(_fscape, ugbCtx, config, force = false)
      }
      OutputGenView(config, output, fscView)
    }
  }
}
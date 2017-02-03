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

import at.iem.sysson.fscape.graph.{Dim, Var}
import at.iem.sysson.sound.AuralSonification
import de.sciss.fscape.lucre.FScape.{Output, Rendering}
import de.sciss.fscape.lucre.UGenGraphBuilder.{IO, Input, MissingIn}
import de.sciss.fscape.lucre.impl.{RenderingImpl, UGenGraphBuilderContextImpl}
import de.sciss.fscape.lucre.{FScape, OutputGenView}
import de.sciss.fscape.stream.Control
import de.sciss.lucre.matrix.{DataSource, Matrix}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.{GenContext, GenView, WorkspaceHandle}

object GenViewFactory {
  def apply(config: Control.Config = Control.Config()): GenView.Factory = new Impl(config)

  def install(config: Control.Config = Control.Config()): Unit =
    GenView.addFactory(apply(config))

  def render[S <: Sys[S]](fscape: FScape[S], config: Control.Config = Control.Config())
                         (implicit tx: S#Tx, context: GenContext[S]): FScape.Rendering[S] = {
    val ugbCtx = new ContextImpl(fscape)
    RenderingImpl(fscape, ugbCtx, config, force = true)
  }

  private final class ContextImpl[S <: Sys[S]](protected val fscape: FScape[S])(implicit context: GenContext[S])
    extends UGenGraphBuilderContextImpl[S] {

    implicit protected def cursor    : stm.Cursor[S]       = context.cursor
    implicit protected def workspace : WorkspaceHandle[S]  = context.workspaceHandle

    private def findMatrix(vr: Var)(implicit tx: S#Tx): Matrix[S] = {
      val f       = fscape
      val vrName  = vr.name
      val mOpt0   = f.attr.$[Matrix](vrName)
      val mOpt    = mOpt0.orElse {
        for {
          sonif  <- AuralSonification.find[S]()
          source <- sonif.sources.get(vrName)
        } yield source.matrix
      }
      val m     = mOpt.getOrElse(throw MissingIn(vrName))
      m
    }

    private def requestDim(dim: Dim)(implicit tx: S#Tx): (Matrix[S], Int) = {
      val f       = fscape
      val vrName  = dim.variable.name
      val dimNameL= dim.name
      val mOpt0   = f.attr.$[Matrix](vrName)
      val mOpt    = mOpt0.fold {
        for {
          sonif   <- AuralSonification.find[S]()
          source  <- sonif.sources.get(vrName)
          dimName <- source.dims.get(dimNameL)
          m        = source.matrix
          dimIdx   = m.dimensions.indexWhere(_.name == dimName)
          if dimIdx >= 0
        } yield (m, dimIdx)
      } { _m =>
        val dimIdx = _m.dimensions.indexWhere(_.name == dimNameL)
        if (dimIdx < 0) None else Some((_m, dimIdx))
      }
      mOpt.getOrElse(throw MissingIn(vrName))
    }

    private def requestDimInfo(i: Dim.InfoGE)(implicit tx: S#Tx): Dim.Info = {
      val (m, dimIdx) = requestDim(i.dim)
      val d = m.dimensions.apply(dimIdx)

      val res: Dim.Info = new Dim.Info {
        val variable: Var.Info    = new Var.Info {
          val matrix  : Matrix.Key  = m.getKey(-1)
          val shape   : Vec[Int]    = m.shape
          val name    : String      = m.name
          val units   : String      = m.units
        }

        val index   : Int         = dimIdx
        val matrix  : Matrix.Key  = m.getDimensionKey(dimIdx, useChannels = false)
        val shape   : Vec[Int]    = d.shape
        val name    : String      = d.name
        val units   : String      = d.units
      }
      res
    }

    private def requestVarSpec(i: Var.Spec)(implicit tx: S#Tx): Var.Spec.Value = {
      val m = findMatrix(i.variable)

      implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]

      val dims0 = m.dimensions.map { dim =>
        val reader: Matrix.Reader = dim.reader(-1)
        val lenL  = reader.size
        require(lenL <= 0x7FFFFFFF)
        val len   = lenL.toInt
        val buf   = new Array[Double](len)
        reader.readDouble1D(buf, 0, len)
        Var.Spec.Dim(dim.name, dim.units, buf.toIndexedSeq)
      }
      val spec0 = Var.Spec.Value(m.name, m.units, dims0)

      val spec = (spec0 /: i.ops) {
        case (specIn, Var.Op.Drop(dimRef)) =>
          require(dimRef.variable == i.variable)
          // N.B. Because we may drop multiple times,
          // we have to "relocate" the dimensional index,
          // simply by looking up its name
          val (m0, dimIdx0) = requestDim(dimRef)
          val dimName       = m0.dimensions.apply(dimIdx0).name
          val dimIdx        = specIn.dimensions.indexWhere(_.name == dimName)
          require(dimIdx >= 0)
          specIn.copy(dimensions = specIn.dimensions.patch(dimIdx, Nil, 1))
      }

      spec
    }

    override def requestInput[Res](req: Input {type Value = Res}, io: IO[S])(implicit tx: S#Tx): Res = req match {
      case Var.PlayLinear(vr) =>
        val m     = findMatrix(vr)
        val res   = new Var.PlayLinear.Value {
          val matrix: Matrix.Key = m.getKey(-1) // reader(-1)

          // XXX TODO: `reader` is called from non-txn
          // and thus we need to create it early here. This is not
          // really cool, because it means the file is opened if
          // if just the cache is validated. We should perhaps change
          // in LucreMatrix the API to use `TxnLike` (what workspace-addDependent uses)
          // instead of `S#Tx`, so we can insert a cheap single txn here
          val /* def */ reader: Matrix.Reader = {
            implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]
            matrix.reader()
          }
        }
        res

      case i: Dim.Size      => requestDimInfo(i)
      case i: Dim.SuccSize  => requestDimInfo(i)
      case i: Var.Spec      => requestVarSpec(i)

      case _ => super.requestInput(req, io)
    }
  }

  private final class Impl(config: Control.Config) extends GenView.Factory {
    def typeID: Int = Output.typeID

    type Repr[~ <: Sys[~]] = Output[~]

    def apply[S <: Sys[S]](output: Output[S])(implicit tx: S#Tx, context: GenContext[S]): GenView[S] = {
      val _fscape = output.fscape
      val fscView = context.acquire[Rendering[S]](_fscape) {
        val ugbCtx = new ContextImpl(_fscape)
        RenderingImpl(_fscape, ugbCtx, config, force = false)
      }
      OutputGenView(config, output, fscView)
    }
  }
}
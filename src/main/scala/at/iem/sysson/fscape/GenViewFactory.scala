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

import at.iem.sysson.fscape.graph.Var
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

  private final class ContextImpl[S <: Sys[S]](protected val fscape: FScape[S])(implicit context: GenContext[S])
    extends UGenGraphBuilderContextImpl[S] {

    implicit protected def cursor    : stm.Cursor[S]       = context.cursor
    implicit protected def workspace : WorkspaceHandle[S]  = context.workspaceHandle

    override def requestInput[Res](req: Input {type Value = Res}, io: IO[S])(implicit tx: S#Tx): Res = req match {
      case Var.PlayLinear(vr) =>
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
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

import at.iem.sysson.fscape.graph.{Matrix, UserValue}
import at.iem.sysson.sound.AuralSonification
import de.sciss.equal.Implicits._
import de.sciss.fscape.lucre.FScape.{Output, Rendering}
import de.sciss.fscape.lucre.UGenGraphBuilder.{IO, Input, MissingIn}
import de.sciss.fscape.lucre.impl.{RenderingImpl, UGenGraphBuilderContextImpl}
import de.sciss.fscape.lucre.{FScape, OutputGenView, UGenGraphBuilder}
import de.sciss.fscape.stream.Control
import de.sciss.lucre.matrix.impl.UGBContextImpl
import de.sciss.lucre.matrix.{Matrix => LMatrix}
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.{GenContext, GenView}

import scala.concurrent.ExecutionContext

object GenViewFactory {
  def apply  (config: Control.Config = FScape.defaultConfig): GenView.Factory = new Impl(config)
  def install(config: Control.Config = FScape.defaultConfig): Unit            = GenView.addFactory(apply(config))

  def render[S <: Sys[S]](fscape: FScape[S], config: Control.Config = FScape.defaultConfig)
                         (implicit tx: S#Tx, context: GenContext[S]): FScape.Rendering[S] = {
    import config.executionContext
    val ugbCtx: UGenGraphBuilder.Context[S] = new ContextImpl(fscape)
    RenderingImpl(fscape, ugbCtx, config, force = true)
  }

  private final class ContextImpl[S <: Sys[S]](protected val fscape: FScape[S])
                                              (implicit protected val gen: GenContext[S],
                                               protected val executionContext: ExecutionContext)
    extends UGenGraphBuilderContextImpl[S] with UGBContextImpl[S] {

    protected def findMatrix(vr: Matrix)(implicit tx: S#Tx): LMatrix[S] = {
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

    protected def requestDim(vrName: String, dimNameL: String)(implicit tx: S#Tx): Option[(LMatrix[S], Int)] = {
      val f       = fscape
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
      mOpt
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
      case i: UserValue => requestUserValue(i)
      case _ => super.requestInput(req, io)
    }
  }

  private final class Impl(config: Control.Config) extends GenView.Factory {
    def typeID: Int = Output.typeID

    type Repr[~ <: Sys[~]] = Output[~]

    def apply[S <: Sys[S]](output: Output[S])(implicit tx: S#Tx, context: GenContext[S]): GenView[S] = {
      val _fscape = output.fscape
      val fscView = context.acquire[Rendering[S]](_fscape) {
        import config.executionContext
        val ugbCtx: UGenGraphBuilder.Context[S] = new ContextImpl(_fscape)
        RenderingImpl(_fscape, ugbCtx, config, force = false)
      }
      OutputGenView(config, output, fscView)
    }
  }
}
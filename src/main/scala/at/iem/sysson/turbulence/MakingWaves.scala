/*
 *  MakingWaves.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.turbulence

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import de.sciss.synth
import de.sciss.synth.{proc, SynthGraph}
import de.sciss.synth.proc.{Ensemble, SoundProcesses, Action, Code, ExprImplicits, FolderElem, Obj, Proc, Folder, graph}
import proc.Implicits._

import scala.concurrent.Future

object MakingWaves {
  // (2)
  def apply[S <: Sys[S]](parent: Folder[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], compiler: Code.Compiler): Future[Unit] = {
    val actionFut = mkAction()
    import Folder.serializer
    val parentH   = tx.newHandle(parent)
    import SoundProcesses.executionContext
    actionFut.map { actionH =>
      cursor.step { implicit tx =>
        val all     = mkWorld(actionH())
        val parent  = parentH()
        for {
          Ensemble.Obj(layers) <- parent / "layers"
        } {
          layers.elem.peer.folder.addLast(all)
        }
      }
    }
  }

  private def mkWorld[S <: Sys[S]](ping: Action[S])
                                  (implicit tx: S#Tx, cursor: stm.Cursor[S]): Proc.Obj[S] = {
    println("Making Waves Proc")
    val wavesGraph = SynthGraph {
      import synth._
      import ugen._
      import Turbulence._

      val period      = graph.Attribute.kr("period", 5)     // seconds between polls
      val ripple      = graph.Attribute.kr("ripple", 0.1)   // percent

      val sense       = SensorIn(0, SensorSpeakers.size)
      val frequencies = sense.linexp(0, 1, 1.0/4, 4.0)
      val sin         = SinOsc.kr(frequencies)
      val sqr         = sense.squared
      val sig         = sin.madd(sqr * ripple, sqr)

      // between all speakers (rounded up)
      val maxDist     = 1206.7 // equalized pixels
      val maxDelay    = 30.0   // seconds
      val maxAtt      = -24.0  // decibels

      val mis = Channels.map { spkA =>
        val ptA  = ChannelToMatrixMap(spkA).toPoint.equalize
        val coll = SensorSpeakers.zipWithIndex.collect {
          case ((spkB, true), sIdx) =>
            val in   = sig \ sIdx
            if (spkA == spkB) in else {
              val ptB  = ChannelToMatrixMap(spkB).toPoint.equalize
              val dist = ptA distanceTo ptB
              val dt   =  dist / maxDist * maxDelay
              val att  = (dist / maxDist * maxAtt).dbamp
              DelayN.kr(in, dt, dt) * att
            }
        }

        // aka coll.maxIndex
        val mi = ArrayMax.kr(coll).index
        mi // .poll(Impulse.kr(period.reciprocal), s"max-$spkA")
      }
      graph.Reaction(Impulse.kr(period.reciprocal), mis, "ping")
    }

    val imp = ExprImplicits[S]
    import imp._

    val proc          = Proc[S]
    proc.graph()      = wavesGraph
    val procObj       = Obj(Proc.Elem(proc))
    procObj.name      = "waves"
    procObj.attr.put("ping", Obj(Action.Elem(ping)))

    //    val sFolder       = Folder[S]
    //    val sFolderObj    = Obj(FolderElem(sFolder))
    //    sFolderObj.name   = "sensors"
    //    parent.addLast(sFolderObj)

    procObj
    // parent.addLast(procObj)
  }

  private def mkAction[S <: Sys[S]]()(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                      compiler: Code.Compiler): Future[stm.Source[S#Tx, Action[S]]] = {
    println("Making Waves Action")
    val source =
      """import at.iem.sysson.turbulence._
        |import Turbulence.NumChannels
        |import MakeWorkspace.DEBUG
        |val imp = ExprImplicits[S]
        |import imp._
        |
        |val sense = values.map(_.toInt)
        |// val debug = self.attr[BooleanElem]("debug").exists(_.value)
        |
        |if (DEBUG) println(sense.map(_.toHexString).mkString("sense: [", ",", "]"))
        |
        |for {
        |  Ensemble.Obj(layers) <- root / "layers"
        |  free                 <- layers.attr[BooleanElem]("free-voices")
        |  Proc.Obj(diff)       <- layers / "diff"
        |  li                   <- 0 until VoiceStructure.NumLayers
        |  Ensemble.Obj(lObj)   <- layers / s"layer-$li"
        |  Expr.Var(transId)    <- lObj.attr[IntElem]("trans")
        |} {
        |      val l               = lObj.elem.peer
        |      val isActive        = l.playing.value
        |      val mayBecomeActive = !isActive && free.value
        |
        |      val hasFadeIn = (isActive || mayBecomeActive) && (false /: (0 until NumChannels)) {
        |        case (res, si) =>
        |          val byName  = s"by${li}_$si"
        |          val res1Opt = for {
        |            Ensemble.Obj(bEns) <- lObj / byName
        |            Proc.Obj(bProc)    <- bEns / byName
        |            Expr.Var(state)    <- bProc.attr[IntElem]("state")
        |          } yield {
        |            val gate    = sense(si) == li
        |            val before  = state.value
        |            val now     = before match {
        |              case 0 | 3 if  gate => 2
        |              case 1 | 2 if !gate => 3
        |              case _ => before
        |            }
        |            if (now != before) {
        |              state() = now
        |              println(s"LAYER ${li}_$si. gate = $gate, before = $before, now = $now")
        |            }
        |            res | (now == 2)
        |          }
        |          res1Opt.getOrElse(res)
        |      }
        |      val becomesActive = mayBecomeActive && hasFadeIn
        |      if (DEBUG) println(s"Layer $li isActive? $isActive; mayBecomeActive? $mayBecomeActive; hasFadeIn? $hasFadeIn.")
        |
        |      if (becomesActive) {
        |        val tid = VoiceStructure.rrand(0, VoiceStructure.NumTransitions - 1) // XXX TODO
        |        transId.update(tid)
        |        for {
        |          Proc.Obj(pred) <- lObj / s"pred$li"
        |          Proc.Obj(out ) <- lObj / s"foo$li"
        |        } {
        |          println(s"LINK $li")
        |          (pred, out).linkBefore(diff)
        |          lObj.play()
        |        }
        |      }
        |}
        |""".stripMargin
    val code  = Code.Action(source)
    Action.compile[S](code)
  }
}

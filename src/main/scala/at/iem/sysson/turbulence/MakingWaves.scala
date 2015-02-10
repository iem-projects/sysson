/*
 *  MakingWaves.scala
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

package at.iem.sysson.turbulence

import de.sciss.kollflitz.RandomOps
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm
import de.sciss.synth
import de.sciss.synth.{proc, SynthGraph}
import de.sciss.synth.proc.{IntElem, BooleanElem, Ensemble, SoundProcesses, Action, Code, ExprImplicits, Obj, Proc, Folder, graph}
import proc.Implicits._

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.Future
import scala.util.{Try, Random}

import VoiceStructure.NumChannels

object MakingWaves {
  final val MaxFadeIns = 8  // at once

  final val BackgroundThresh  = sys.props.get("turbulence-bg-thresh").flatMap(s => Try(s.toDouble).toOption)
    .getOrElse(4.0e-4)

  final val BackgroundSum     = sys.props.get("turbulence-bg-sum"   ).flatMap(s => Try(s.toDouble).toOption)
    .getOrElse(7.5e-4)

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

  var overrideValue = -1 // -1 to _not_ override, otherwise layer index

  private def mkWorld[S <: Sys[S]](ping: Action[S])
                                  (implicit tx: S#Tx, cursor: stm.Cursor[S]): Proc.Obj[S] = {
    println("Making Waves Proc")
    val wavesGraph = SynthGraph {
      import synth._
      import ugen._
      import Turbulence._

      val period      = graph.Attribute.kr("period", 5)     // seconds between polls
      val ripple      = graph.Attribute.kr("ripple", 0.1)   // percent

      val sense0      = SensorIn(0, SensorSpeakers.size)
      val sense: GE   = Vec.tabulate(SensorSpeakers.size) { i =>
        val in = sense0 \ i
        val th = sys.props.get(s"turbulence-s$i-thresh").flatMap(s => Try(s.toDouble).toOption).getOrElse(0.0)
        if (th > 0) println(f"For sensor T$i%02d threshold is $th%1.5f")
        if (th <= 0) in else (in - th).max(0)
      }
      val frequencies = sense.linexp(0, 1, 1.0/4, 4.0)
      val sin         = SinOsc.kr(frequencies)
      val sqr         = sense.squared
      val sig         = sin.madd(sqr * ripple, sqr)

      // between all speakers (rounded up)
      val maxDist     = 1206.7 // equalized pixels
      val maxDelay    = 60.0 // 30.0   // seconds
      val maxAtt      = -24.0  // decibels

      val mis = Channels.flatMap { spkA =>
        val ptA  = ChannelToMatrixMap(spkA).toPoint.equalize
        val coll = SensorSpeakers.zipWithIndex.collect {
          case ((spkB, true), sIdx) =>
            val in   = sig \ sIdx
            if (spkA == spkB) in else {
              val ptB  = ChannelToMatrixMap(spkB).toPoint.equalize
              val dist = ptA distanceTo ptB
              val dt   =  dist / maxDist * maxDelay
              val att  = (dist / maxDist * maxAtt).dbamp
              // val buf  = LocalBuf((dt * 44100).ceil.toInt)
              // BufDelayN.kr(buf, in, dt) * att
              DelayN.kr(in, dt, dt) * att
            }
        }

        // aka coll.maxIndex
        val am = ArrayMax.kr(coll)
        val mi = am.index
        val mv = am.value
        Seq(mi, mv) // .poll(Impulse.kr(period.reciprocal), s"max-$spkA")
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

  private lazy val ChanSeq = (0 until NumChannels).toIndexedSeq

  private implicit val rnd = new Random() // it's ok, leave it seeded with clock

  def react[S <: Sys[S]](values: Vec[Float], root: Folder[S])(implicit tx: S#Tx): Unit = {
    import MakeWorkspace.DEBUG
    val imp = ExprImplicits[S]
    import imp._

    val sense: Vec[Int] = if (overrideValue < 0) {
      val sumEnergy = (0.0 /: (0 until NumChannels)) { case (sum, si) =>
        val sit = si << 1  // offset for [sensor-id, sensor-value]
        sum + values(sit + 1)
      }
      // println(s"SUM ENERGY = $sumEnergy")
      val lowEnergy = sumEnergy < BackgroundSum
      Vector.tabulate(NumChannels) { si =>
        val sit     = si << 1
        val senseID = values(sit).toInt // + 1
        val li      = MakeLayers.map.getOrElse(senseID, -1)
        if (lowEnergy && values(sit + 1) < BackgroundThresh) 0 else li
      }
    } else {
      Vector.fill(NumChannels)(overrideValue)
    }

    if (DEBUG) println(sense.map(_.toHexString).mkString("sense: [", ",", "]"))

    for {
      Ensemble.Obj(layers) <- root / "layers"
      free                 <- layers.attr[BooleanElem]("free-voices")
      Proc.Obj(diff)       <- layers / "diff"
      li                   <- 0 until VoiceStructure.NumLayers
      Ensemble.Obj(lObj)   <- layers / s"layer-$li"
      Expr.Var(transId)    <- lObj.attr[IntElem]("trans")
    } {
      val l               = lObj.elem.peer
      val isActive        = l.playing.value
      val mayBecomeActive = !isActive && free.value

      var numFadeIns  = 0 // dirty...

      import RandomOps._
      // implicit val rnd = new Random(sense.hashCode())
      // val chans = (0 until NumChannels).toUrn

      val hasFadeIn = (isActive || mayBecomeActive) && (false /: ChanSeq.toUrn(infinite = false)) {
        case (res, si) =>
          val byName  = s"by${li}_$si"
          val res1Opt = for {
            Ensemble.Obj(bEns) <- lObj / byName
            Proc.Obj(bProc)    <- bEns / byName
            Expr.Var(state)    <- bProc.attr[IntElem]("st")
          } yield {
            val li2     = sense(si)
            val gate    = li2 == li
            val before  = state.value
            val now     = before match {
              case 0 | 3 if gate && numFadeIns < MaxFadeIns =>
                numFadeIns += 1
                2
              case 1 | 2 if !gate => 3
              case _ => before
            }
            if (now != before) {
              state() = now
              if (DEBUG) println(s"LAYER ${li}_$si. gate = $gate, before = $before, now = $now")
            }
            res | (now == 2)
          }
          res1Opt.getOrElse(res)
      }
      val becomesActive = mayBecomeActive && hasFadeIn
      if (DEBUG) println(s"Layer $li isActive? $isActive; mayBecomeActive? $mayBecomeActive; hasFadeIn? $hasFadeIn.")

      if (becomesActive) {
        val tid = VoiceStructure.rrand(0, VoiceStructure.NumTransitions - 1)
        transId.update(tid)
        for {
          Proc.Obj(pred) <- lObj / s"pred$li"
          Proc.Obj(out ) <- lObj / s"foo$li"
        } {
          if (DEBUG) println(s"LINK $li")
          (pred, out).linkBefore(diff)
          lObj.play()
        }
      }
    }
  }

  private def mkAction[S <: Sys[S]]()(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                      compiler: Code.Compiler): Future[stm.Source[S#Tx, Action[S]]] = {
    println("Making Waves Action")
    val source =
      """import at.iem.sysson.turbulence._
        |MakingWaves.react(values, root)
        |""".stripMargin
    val code = Code.Action(source)
    Action.compile[S](code)
  }
}

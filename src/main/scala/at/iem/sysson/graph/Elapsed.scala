/*
 *  Elapsed.scala
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
package graph

import at.iem.sysson.sound.AuralSonification
import de.sciss.lucre.stm
import de.sciss.lucre.synth.{DynamicUser, Node, Sys, Txn}
import de.sciss.synth.proc.{UGenGraphBuilder => UGB, SoundProcesses, AuralContext}
import de.sciss.synth.{HasSideEffect, Rate, UGenInLike, audio, control, message}
import de.sciss.{osc, synth}

import scala.concurrent.stm.Ref

/** A special graph element that measures the progress of a playing dimension.
  * Additionally it can stop the playing synth when that progress has reached 100%.
  */
object Elapsed {
  /** Convenient syntax if the output of the element is not used.
    * E.g. `Elapsed := aPlayingDim` will monitor the dimension and
    * stop the synth when its time has elapsed.
    */
  def :=(in: Dim.Play): synth.GE = ar(in)

  /** @param in         the playing dimension
    * @param terminate   if `true`, stops the synth when 100% is reached
    */
  def ar(in: Dim.Play, terminate: Boolean = true): synth.GE = Elapsed(in).ar(terminate = terminate)

  /** @param in         the playing dimension
    * @param terminate   if `true`, stops the synth when 100% is reached
    */
  def kr(in: Dim.Play, terminate: Boolean = true): synth.GE = Elapsed(in).kr(terminate = terminate)

  private def replyName /* (key: String) */: String = "/$elpsd" // s"/$$act_$key"

  /** Graph element that actually reports the progress
    *
    * @param terminate   if `true`, stops the synth when 100% is reached
    */
  case class GE(rate: Rate, peer: Elapsed, terminate: Boolean) extends synth.GE.Lazy with HasSideEffect {
    import peer.in

    override def productPrefix  = "Elapsed$GE"
    override def toString       = {
      val suffix = if (terminate) "(terminate = true)" else ""
      s"Elapsed($in).${rate.methodName}$suffix"
    }

    protected def makeUGens: UGenInLike = {
      val b         = UGB.get
      b.requestInput(peer)
      val reportID  = 0 // proc.graph.Attribute.ir(key)
      import synth._
      import synth.ugen._
      val frame     = Sweep(rate, 0, in.freq) // speed = increment _per second_
      val numFrames = peer.in.dim.size
      val ratio     = frame / numFrames
      //        in.freq      .poll(0, "freq")
      //        frame        .poll(2, "frame")
      //        numFrames    .poll(0, "numFrames")

      val report    = Impulse(rate, 20)   // XXX TODO --- rate could be configurable
      val reportSig = Seq(ratio, in): synth.GE
      SendReply(rate, report, reportSig, replyName, id = reportID)
      if (terminate) {
        val done    = ratio >= 1
        // because the `report` pulse might not fire exactly at 100%,
        // and the synth goes to pause at 100%, we must unfortunately
        // create a second `SendReply` for that singular event.
        // (it's also not safe to add the two triggers, because we might
        // have no new trigger at exactly 100%)
        SendReply(rate, done, Seq(ratio, in), "/$elpsd", id = reportID)
        val doneK   = if (rate == audio) A2K.kr(done) else done
        // the aural-sonif will stop the transport. what we must
        // ensure is that the sounds stops immediately. to avoid
        // annoying 'node not found' message, pause instead of free the synth.
        PauseSelf.kr(doneK)
      }
      ratio
    }
  }

  // case object Key extends UGB.Key

  private[sysson] class Responder[S <: Sys[S]](key: graph.Dim, status: stm.Sink[S#Tx, AuralSonification.Update /* [S] */],
                                               synth: Node)
                                              (implicit context: AuralContext[S])
    extends DynamicUser {

    private val Name    = replyName // (key)
    private val NodeID  = synth.peer.id
    private val trigResp = message.Responder(synth.server.peer) {
      case m @ osc.Message(Name, NodeID, 0, ratio: Float, dimValue: Float) =>
        import context.scheduler.cursor
        SoundProcesses.atomic { implicit tx: S#Tx =>
          // println(f"elapsed - ${ratio * 100}%1.1f - $dimValue%1.3f")
          status() = AuralSonification.Elapsed(key = key, ratio = ratio, dimValue = dimValue)
        }
    }

    private val added = Ref(initialValue = false)

    def add()(implicit tx: Txn): Unit = if (!added.swap(true)(tx.peer)) {
      // println("ADD ELAPSED RESPONDER")
      trigResp.add()
      // Responder.add is non-transactional. Thus, if the transaction fails, we need to remove it.
      scala.concurrent.stm.Txn.afterRollback { _ =>
        trigResp.remove()
      } (tx.peer)
    }

    def remove()(implicit tx: Txn): Unit = if (added.swap(false)(tx.peer)) {
      // println("REMOVE ELAPSED RESPONDER")
      trigResp.remove()
      scala.concurrent.stm.Txn.afterRollback { _ =>
        trigResp.add()
      } (tx.peer)
    }
  }
}

/** A special element that measures the progress of a playing dimension.
  *
  * @param in   the playing dimension
  */
final case class Elapsed(in: Dim.Play)
  extends UserInteraction with UGB.Input with UGB.Key {

  type Key      = Elapsed // .Key.type
  type Value    = UGB.Unit

  def key: Key  = this // Elapsed.Key

  def ar: synth.GE = ar(terminate = true)
  def ar(terminate: Boolean): synth.GE = Elapsed.GE(audio  , this, terminate = terminate)
  def kr: synth.GE = kr(terminate = true)
  def kr(terminate: Boolean): synth.GE = Elapsed.GE(control, this, terminate = terminate)
}
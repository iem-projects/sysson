/*
 *  SonificationImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound
package impl

import de.sciss.synth._
import concurrent.duration.Duration
import io.{AudioFileSpec, AudioFile}
import java.io.File
import Ops._
import de.sciss.{synth, osc}
import de.sciss.file._
import scala.concurrent.{ExecutionContext, Promise, Future, future, blocking}
import Implicits._
import de.sciss.synth.swing.SynthGraphPanel
import at.iem.sysson.gui.GUI

object SonificationOLDImpl {
  var DEBUG_GRAPH     = false
  var DEBUG_WRITEDEF  = false

  private final val synthDefName = "$son_play"

  def apply(name: String): SonificationOLD = new Impl(name)

  private final val codec = osc.PacketCodec().scsynth().build

  private final class PreparedBuffer(val section: UGenGraphBuilder.Section, val file: File, val spec: AudioFileSpec)

  private final class SynthPreparation(val synth: Synth) {
    var msgs  = Vec.empty[osc.Message]
    var ctls  = Vec.empty[ControlSetMap]
  }

  private final class Impl(var name: String) extends SonificationOLD {

    var mapping       = Map.empty[String, SonificationSource]
    var variableMap   = Map.empty[String, VariableSection   ]
    var userValueMap  = Map.empty[String, Double            ]

    override def toString = s"Sonification($name)${hashCode().toHexString}"

    var patch = PatchOLD.empty

    def playOver(duration: Duration): Synth = {
      //      mapping.values.headOption.map(_.numColumns)
      val szs = mapping.values.collect {
        case r @ RowSource(_)           => r.numColumns
        case m @ MatrixSource(_, _, _)  => m.numColumns
      }

      //println(s"szs.headOption = ${szs.headOption}")

      szs.headOption match {
        case Some(sz) =>
          val rate  = sz.toDouble * 1000 / duration.toMillis
          play(rate = rate, duration = None)

        case None =>
          play(rate = 1.0, duration = Some(duration.toMillis * 0.001))
      }
      //      val sz    = szs.headOption.getOrElse(sys.error("playOver requires at least one row vector"))
    }

    def play(rate: Double): Synth = play(rate = rate, duration = None)

    private def buildUGens(duration: Option[Double]): UGenGraphBuilder.Result =
      UGenGraphBuilder(this,
        patch.graph
        //        SynthGraph {
        //          import synth._
        //          import ugen._
        //          val res = _graph()
        //          val sig = duration match {
        //            case Some(secs) if secs > 0 =>
        //              val sus = math.max(0, secs - 0.02)
        //              val rls = secs - sus
        //              val env = Env.linen(attack = 0, sustain = sus, release = rls, curve = Curve.sine)
        //              res * EnvGen.ar(env, doneAction = freeSelf)
        //            case _ => res
        //          }
        //          WrapOut(in = sig, fadeTime = Some(0.02f))
        //        }
      )

    def prepare()(implicit context: ExecutionContext): Future[SonificationOLD.Prepared] = {
      val as  = AudioSystem.instance
      if (!as.isBooting && !as.isRunning) as.start() // this can start up during our preparations

      val prepared: Future[(UGenGraphBuilder.Result, Vec[PreparedBuffer])] = future {
        val res = buildUGens(duration = None)

        if (DEBUG_GRAPH) {
          GUI.defer {
            val p = new SynthGraphPanel(patch.name, res.graph, forceDirected = true)
            p.makeWindow().setVisible(true)
          }
        }
        if (DEBUG_WRITEDEF) _debug_writeDef()

        res -> res.sections.map { section =>
          blocking {
            val arr = section.peer.read()
            val n   = arr.getSize
            // if using streaming, we need probably:
            // arr.transpose()

            // cf. Arrays.txt for (de-)interleaving scheme

            // - when not streaming, we use a monophonic linearised buffer (instead of a buffer
            //   with one frame and arr.size channels).
            // - when streaming, we use a channel per rank-product (excluding time dimension),
            //   possibly allowing interpolations in BufRd.
            val numFrames     = if (!section.isStreaming) n else arr.getIndexPrivate.getShape(section.streamDim)
            val numChannelsL  = n / numFrames
            require (numChannelsL <= 4096, s"The section $section would require too large number of channels ($numChannelsL > 4096)")
            val numChannels   = numChannelsL.toInt

            // XXX TODO: for small amounts of data, send it straight to the buffer using b_set
            // instead going via a sound file which is more costly in terms of latency

            val file          = File.createTempFile("sysson", ".aif")
            // val path          = file.getAbsolutePath

            // WARNING: sound file should be AIFF to allow for floating point sample rates
            // (note: sample rate not used now)
            val af            = AudioFile.openWrite(file, AudioFileSpec(numChannels = numChannels, sampleRate = 44100 /* rate */))
            val fBufSize      = math.min(8192/numChannels, numFrames).toInt // file buffer
            assert(fBufSize > 0)
            val fBuf          = af.buffer(fBufSize)
            var framesWritten = 0L
            val t             = if (section.streamDim <= 0) arr else arr.transpose(0, section.streamDim)
            val it            = t.float1Diterator
            while (framesWritten < numFrames) {
              val chunk = math.min(fBufSize, numFrames - framesWritten).toInt
              var i = 0
              while (i < chunk) {
                var ch = 0
                while (ch < numChannels) {
                  fBuf(ch)(i) = it.next() // XXX TODO: would be better to have inner loop iterate for frames
                  ch += 1
                }
                i += 1
              }
              af.write(fBuf, 0, chunk)
              framesWritten += chunk
            }
            af.close()

            new PreparedBuffer(section, file, af.spec)
          }
        }
      }

      val p = Promise[SonificationOLD.Prepared]()
      as.whenBooted { s =>
        p completeWith prepared.map { case (res, buffers) =>
          new SonificationOLD.Prepared {
            def play(): Synth = {
              val sd    = SynthDef(synthDefName, res.graph)
              val syn   = Synth(s)
              val prep  = new SynthPreparation(syn)

              // userValueMap.foreach { case (key, value) =>
              //   prep.ctls :+= (key -> value: ControlSetMap)
              // }

              res.userValues.foreach { uv =>
                userValueMap.get(uv.peer.key).foreach { value =>
                  prep.ctls :+= (uv.controlName -> value: ControlSetMap)
                }
              }

              buffers.foreach { b =>
                val sBuf  = Buffer(s)
                syn.onEnd {
                  if (b.section.isStreaming) sBuf.close() // file was streamed
                  sBuf.free()
                  b.file.delete()
                }

                prep.ctls :+= (b.section.controlName -> sBuf.id: ControlSetMap)

                if (b.section.isStreaming) {
                  prepareStreaming(prep, sBuf, b)

                } else {
                  val spec        = b.spec
                  val numFrames  = spec.numFrames
                  require (numFrames < 0x80000000L, s"Number of frames for $b.section is too large")
                  val allocMsg    = sBuf.allocMsg(numFrames = numFrames.toInt, numChannels = spec.numChannels)
                  val readMsg     = sBuf.readMsg(b.file.absolutePath)
                  prep.msgs     :+= allocMsg
                  prep.msgs     :+= readMsg
                }
              }

              // println(prep.ctls)

              val newMsg  = syn.newMsg(synthDefName, args = prep.ctls)
              val recvMsg = sd.recvMsg(newMsg)
              val dfMsg   = if (codec.encodedMessageSize(recvMsg) < 0x3FFF) recvMsg else {
                sd.write(overwrite = true)
                sd.loadMsg(completion = newMsg)
              }
              //      val bndl    = if (msgs.isEmpty) newMsg else {
              //        val init = msgs.init
              //        val last = msgs.last
              //        val upd  = last.updateCompletion(Some(recvMsg))
              //        osc.Bundle.now((init :+ upd): _*)
              //      }
              val bndl  = osc.Bundle.now(prep.msgs :+ dfMsg: _*)

              s ! bndl
              syn
            }
          }
        }
      }

      p.future
    }

    private def prepareStreaming(prep: SynthPreparation, sBuf: Buffer, b: PreparedBuffer): Unit = {
      val spec          = b.spec
      val numFrames     = spec.numFrames
      val numChannels   = spec.numChannels

      val rate = 44100.0/64   // XXX TODO how to determine the max frequency !?

      // at least 32 samples, at most one second with respect to maximum assumed playback rate,
      // unless total file length is smaller.
      val bufSizeHM     = math.max(32, math.min((numFrames + 1)/2, math.ceil(rate).toInt).toInt)
      val bufSizeH      = bufSizeHM + UGenGraphBuilderImpl.diskPad
      val bufSize       = bufSizeH * 2

      // ---- buffer updating function ----

      // 'flatten'
      def addPacket(p: osc.Packet) {
        p match {
          case osc.Bundle(_, pp @ _*)         => pp.foreach(addPacket)
          case m @ osc.Message(_, args @ _*)  => prep.msgs :+= m
        }
      }

      def updateBuffer(trigVal: Int): (osc.Packet, Int) = {
        val trigEven    = trigVal % 2 == 0
        val bufOff      = if (trigEven) 0 else bufSizeH
        val frame       = trigVal * bufSizeHM /* + startPos = 0 */ + (if (trigEven) 0 else UGenGraphBuilderImpl.diskPad)
        val readSz      = math.max(0, math.min(bufSizeH, numFrames - frame)).toInt
        val fillSz      = bufSizeH - readSz
        var ms          = List.empty[osc.Packet]

        if (fillSz > 0) {
          val m = sBuf.fillMsg((bufOff + readSz) * numChannels, fillSz * numChannels, 0f)
          ms = m :: ms
        }

        if (readSz > 0) {
          val m = sBuf.readMsg(
            path            = b.file.absolutePath,
            fileStartFrame  = frame,
            numFrames       = readSz,
            bufStartFrame   = bufOff,
            leaveOpen       = false
          )
          ms = m :: ms
        }

        val p = ms match {
          case single :: Nil  => single
          case _              => osc.Bundle.now(ms: _*)
        }

        (p, frame)
      }

      // ---- register trig responder ----

      val s         = sBuf.server
      val trigResp  = message.Responder.add(s) {
        case osc.Message("/tr", _ /* syn.id */, _ /* GraphB.diskTrigID */, trigValF: Float) =>
          //                println(s"RECEIVED TR $trigValF...")
        val trigVal = trigValF.toInt + 1
          val (p, frame) = updateBuffer(trigVal)
          //                println(s"...translating to frame $frame of $numFrames")
          if (frame < numFrames + bufSizeH) {
            s ! p
          } else {
            prep.synth.free()
          }
      }

      // ---- generate initial buffer updates ----
      val allocMsg      = sBuf.allocMsg(numFrames = bufSize, numChannels = numChannels)
      prep.msgs       :+= allocMsg
      addPacket(updateBuffer(0)._1)
      addPacket(updateBuffer(1)._1)

      prep.synth.onEnd { trigResp.remove() }
    }

    private def play(rate: Double, duration: Option[Double]): Synth = {
      val s = AudioSystem.instance.server match {
        case Some(_s: Server) => _s
        case _ => sys.error("Audio system not running")
      }
      validateMatrixSpecs()
      val res   = buildUGens(duration = duration)
      val sd    = SynthDef(synthDefName, res.graph)
      val syn   = Synth(s)
      var ctls  = Vector.empty[ControlSetMap]
      var msgs  = Vector.empty[osc.Message] // osc.Message with message.HasCompletion]

      val keySet  = Set.empty[String] // XXX TODO

      keySet.foreach { key =>
        val source      = mapping(key)
        val numChannels = source.numRows
        val numFrames   = source.numColumns
        val buf         = Buffer(s)
        val file        = File.createTempFile("sysson", ".aif")
        val path        = file.getAbsolutePath
        syn.onEnd {
          if (numFrames == 1) buf.close() // file was streamed
          buf.free()
          file.delete()
        }

        // WARNING: sound file should be AIFF to allow for floating point sample rates
        val af        = AudioFile.openWrite(file, AudioFileSpec(numChannels = numChannels, sampleRate = rate))
        ??? // ctls        :+= (GraphB.bufCtlName(key) -> buf.id: ControlSetMap)
        val data      = source.section.readScaled1D() // XXX TODO: should read piece wise for large files

        try {
          if (numFrames == 1) {
            val fBuf      = af.buffer(1)
            data.zipWithIndex.foreach { case (f, ch) => fBuf(ch)(0) = f }
            af.write(fBuf)
            val allocMsg  = buf.allocMsg(numFrames = 1, numChannels = numChannels)
            val readMsg   = buf.readMsg(path)
            msgs      :+= allocMsg
            msgs      :+= readMsg

          } else { // row or matrix source; requires streaming
            val bufSizeHM     = math.max(32, math.ceil(rate).toInt)
            val bufSizeH      = bufSizeHM; ??? // + GraphB.diskPad
            val bufSize       = bufSizeH * 2

            val fBufSz        = math.min(8192, numFrames)
            val fBuf          = af.buffer(fBufSz)
            var rem           = numFrames
            val isTrns        = source.isTransposed
            while (rem > 0) {
              val chunk = math.min(fBufSz, rem)
              var ch    = 0
              while (ch < numChannels) {
                val cBuf  = fBuf(ch)
                var fr    = 0
                while (fr < chunk) {
                  val dataIdx = if (isTrns) { // XXX TODO: test if this is correct
                    fr * numFrames + ch
                  } else {
                    ch * numChannels + fr
                  }
                  cBuf(fr) = data(dataIdx)
                  fr += 1
                }
                ch += 1
              }
              af.write(fBuf, 0, chunk)
              rem -= chunk
            }

            // ---- buffer updating function ----
            
            // 'flatten'
            def addPacket(p: osc.Packet) {
              p match {
                case osc.Bundle(_, pp @ _*)         => pp.foreach(addPacket)
                case m @ osc.Message(_, args @ _*)  => msgs :+= m
              }
            }

            def updateBuffer(trigVal: Int): (osc.Packet, Int) = {
              val trigEven    = trigVal % 2 == 0
              val bufOff      = if (trigEven) 0 else bufSizeH
              val frame       = trigVal * bufSizeHM /* + startPos = 0 */ + (if (trigEven) 0 else { ???; 0 } /* GraphB.diskPad */)
              val readSz      = math.max(0, math.min(bufSizeH, numFrames - frame))
              val fillSz      = bufSizeH - readSz
              var ms          = List.empty[osc.Packet]

              if (fillSz > 0) {
                val m = buf.fillMsg((bufOff + readSz) * numChannels, fillSz * numChannels, 0f)
                ms = m :: ms
              }

              if (readSz > 0) {
                val m = buf.readMsg(
                  path            = path,
                  fileStartFrame  = frame,
                  numFrames       = readSz,
                  bufStartFrame   = bufOff,
                  leaveOpen       = false
                )
                ms = m :: ms
              }

              val p = ms match {
                case single :: Nil  => single
                case _              => osc.Bundle.now(ms: _*)
              }

              (p, frame)
            }

            // ---- register trig responder ----

            val trigResp  = message.Responder.add(s) {
              case osc.Message("/tr", _ /* syn.id */, _ /* GraphB.diskTrigID */, trigValF: Float) =>
                //                println(s"RECEIVED TR $trigValF...")
              val trigVal = trigValF.toInt + 1
                val (p, frame) = updateBuffer(trigVal)
                //                println(s"...translating to frame $frame of $numFrames")
                if (frame < numFrames + bufSizeH) {
                  s ! p
                } else {
                  syn.free()
                }
            }

            // ---- generate initial buffer updates ----
            val allocMsg      = buf.allocMsg(numFrames = bufSize, numChannels = numChannels)
            msgs          :+= allocMsg
            addPacket(updateBuffer(0)._1)
            addPacket(updateBuffer(1)._1)

            syn.onEnd { trigResp.remove() }
          }

        } finally {
          af.close()
        }
      }
      val newMsg  = syn.newMsg(synthDefName, args = ctls)
      val recvMsg = sd.recvMsg(newMsg)
      val dfMsg   = if (codec.encodedMessageSize(recvMsg) < 0x3FFF) recvMsg else {
        sd.write(overwrite = true)
        sd.loadMsg(completion = newMsg)
      }
      //      val bndl    = if (msgs.isEmpty) newMsg else {
      //        val init = msgs.init
      //        val last = msgs.last
      //        val upd  = last.updateCompletion(Some(recvMsg))
      //        osc.Bundle.now((init :+ upd): _*)
      //      }
      val bndl  = osc.Bundle.now(msgs :+ dfMsg: _*)

      s ! bndl

      syn
    }

    def _debug_writeDef(): Unit = {
      val dir   = userHome / "Desktop"
      val res   = buildUGens(duration = None)
      val sd    = SynthDef(synthDefName, res.graph)
      sd.write(dir.path, overwrite = true)
    }

    private def validateMatrixSpecs(): Unit = {
      // XXX TODO
      //      mapping.foreach { case (key, source) =>
      //        val spec = matrices.getOrElse(key, sys.error(s"Sonification contains source for unknown key '$key'"))
      //        ...
      //      }
    }
  }
}
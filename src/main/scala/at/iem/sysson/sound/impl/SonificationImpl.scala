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

object SonificationImpl {
  private final val synthDefName = "$son_play"

  def apply(name: String): Sonification = new Impl(name)

  private final val codec = osc.PacketCodec().scsynth().build

  private final class Impl(var name: String) extends Sonification {
    private var _graph = () => 0f: GE
    // var matrices = Map.empty[String, MatrixSpec]

    var mapping     = Map.empty[String, SonificationSource]
    var variableMap = Map.empty[String, VariableSection   ]

    override def toString = s"Sonification($name)${hashCode().toHexString}"

    def graph:     () => GE        = _graph
    def graph_=(body: => GE): Unit = _graph = () => body

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
      UGenGraphBuilder(this, SynthGraph {
        import synth._
        import ugen._
        val res = _graph()
        val sig = duration match {
          case Some(secs) if secs > 0 =>
            val sus = math.max(0, secs - 0.02)
            val rls = secs - sus
            val env = Env.linen(attack = 0, sustain = sus, release = rls, curve = Curve.sine)
            res * EnvGen.ar(env, doneAction = freeSelf)
          case _ => res
        }
        WrapOut(in = sig, fadeTime = Some(0.02f))
      })

    def prepare()(implicit context: ExecutionContext): Future[Sonification.Prepared] = {
      val as  = AudioSystem.instance
      if (!as.isBooting || as.isRunning) as.start() // this can start up during our preparations
      //      val p   = Promise[Sonification.Prepared]()
      //      p.future

      val res = buildUGens(duration = None)
      future {
        res.sections.foreach { case section =>
          // val data = section.readScaled1D()

          // section.toSection
          val v = section.variable
          blocking {
            val arr = section.peer.read()
            val it  = arr.float1Diterator
            val n   = arr.size

          }
        }

        ???
      }
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
      val dir   = file(sys.props("user.home")) / "Desktop"
      val res   = buildUGens(duration = None)
      val sd    = SynthDef(synthDefName, res.graph)
      sd.write(dir.getPath, overwrite = true)
    }

    private def validateMatrixSpecs(): Unit = {
      // XXX TODO
      //      mapping.foreach { case (key, source) =>
      //        val spec = matrices.getOrElse(key, sys.error(s"Sonification contains source for unknown key '$key'"))
      //        ???
      //      }
    }
  }
}
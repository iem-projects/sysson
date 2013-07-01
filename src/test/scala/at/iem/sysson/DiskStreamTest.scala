package at.iem.sysson

import java.io.File
import sound.AudioSystem
import de.sciss.{osc, synth}
import synth._
import ugen._
import io.{AudioFile, AudioFileSpec}
import Ops._
import concurrent.{ExecutionContext, future}
import ExecutionContext.Implicits.global

object DiskStreamTest extends App {
  val f     = File.createTempFile("sysson", ".aif")
  val path  = f.getAbsolutePath
  f.deleteOnExit()

  val seq = Array[Float](1,1,2,3,5,8,0,1,0,1)
  val numChannels = 1
  val numFrames   = seq.length.toLong
  val frameMax    = numFrames.toInt
  val sampleRate  = 2
  val af = AudioFile.openWrite(f, AudioFileSpec(numChannels = numChannels, sampleRate = sampleRate)) // sr does not matter
  af.write(Array(seq))
  af.close()

  val userInterpolation = false

  val cfg = Server.Config()
  cfg.transport = osc.TCP
//  cfg.programPath = "/Applications/SuperCollider_3.5.1/SuperCollider.app/Contents/Resources/scsynth"
  val as = AudioSystem.instance
  as.start(cfg).whenBooted { s =>
//    s.dumpOSC(osc.Dump.Text)

    val pad           = if (userInterpolation) 4 else 0 // 4 if using cubic interpolation
    val halfBufSizeM  = s.sampleRate.toInt
    val halfBufSize   = halfBufSizeM + pad
    val bufSize       = halfBufSize * 2

    val startPos      = 0 // L

    val df = SynthDef("disk") {
      val inBuf       = "buf".ir
      val bufRate     = "rate".ir   // buffer sample rate * playback speed
      val numFrames   = BufFrames.ir(inBuf) // .kr ?
//      val rate        = "rate".kr(1)
      val phasorRate  = bufRate / SampleRate.ir
      val halfPeriod  = numFrames / (bufRate * 2)
      val phasor      = Phasor.ar(speed = phasorRate, lo = pad, hi = numFrames - pad)
      val interp: GE  = if (userInterpolation) (phasorRate - 1.0).signum.abs * 3 + 1 else 1
      val bufReader   = BufRd.ar(numChannels, buf = inBuf, index = phasor, loop = 0, interp = interp)
      val phasorTrig  = Trig1.kr(A2K.kr(phasor) - numFrames/2, ControlDur.ir) //
      val clockTrig   = phasorTrig + TDelay.kr(phasorTrig, halfPeriod)

      SendTrig.kr(clockTrig, value = PulseCount.kr(clockTrig))
      // Out.ar("out".ir, bufReader)
      bufReader.poll(trig = Impulse.ar(4), label = "buf")
    }

    def updateBuffer(buf: Buffer, trigVal: Int, completion: Option[osc.Packet] = None): (osc.Packet, Int) = {
      val trigEven    = trigVal % 2 == 0
      val bufOff      = if (trigEven) 0 else halfBufSize
      val frame       = trigVal * halfBufSizeM + startPos + (if (trigEven) 0 else pad)
      val readSz      = math.max(0, math.min(halfBufSize, frameMax - frame))
      val fillSz      = halfBufSize - readSz
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
          leaveOpen       = false,
          completion      = completion
        )
        ms = m :: ms
      } else completion.foreach { m =>
        ms = m :: ms
      }

      val p = ms match {
        case single :: Nil  => single
        case _              => osc.Bundle.now(ms: _*)
      }

      (p, frame)
    }

    val buf       = Buffer(s)
    val synth     = Synth(s)

    val newMsg    = synth.newMsg(df.name, target = s, args = Seq("buf" -> buf.id, "rate" -> sampleRate))
    val update2   = updateBuffer(buf, trigVal = 1, completion = Some(newMsg))._1
    val update1   = updateBuffer(buf, trigVal = 0, completion = Some(update2))._1
    val allocMsg  = buf.allocMsg(numFrames = bufSize, numChannels = numChannels, completion = Some(update1))
    val recvMsg   = df.recvMsg(completion = Some(allocMsg))

//    val cd = osc.PacketCodec().scsynth().build
//    val bb = ByteBuffer.allocate(65536)
//    update1.encode(cd, bb)
//    bb.flip()
//    val mm1 = cd.decode(bb)

    val trigResp  = message.Responder.add(s) {
      case osc.Message("/tr", synth.id, _, trigValF: Float) =>
        val trigVal = trigValF.toInt + 1
        val (p, frame) = updateBuffer(buf, trigVal = trigVal)
        if (frame < frameMax + halfBufSize) {
          s ! p
        } else {
          synth.free()
          future {
            Thread.sleep(1000)
            println("Quitting...")
            as.stop()
            sys.exit(0)
          }
        }
    }

    synth.onEnd {
      trigResp.remove()
      buf.free()
    }

    s ! recvMsg

//    val cd = osc.PacketCodec().scsynth().build
//    val bb = ByteBuffer.allocate(65536)
//    cd.encodeMessage(recvMsg, bb)
//    bb.flip()
//    val mm1 = cd.decode(bb)
//    mm1 match {
//      case osc.Message("/d_recv", _, bb1: ByteBuffer) =>
//        val mm2 = cd.decode(bb1)
//        mm2 match {
//          case osc.Message("/b_alloc", _, _, _, bb2: ByteBuffer) =>
//            val mm3 = cd.decode(bb2)
//            println(mm3)
//        }
//    }
  }
}
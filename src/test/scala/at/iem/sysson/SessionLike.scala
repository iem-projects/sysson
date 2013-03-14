package at.iem.sysson

import sound.AudioSystem
import Implicits._
import de.sciss.synth
import synth._
import io.SampleFormat
import ugen._
import synth.Ops._

trait SessionLike extends App {
  def run(): Unit

  AudioSystem.start().whenBooted { _ => run() }

  lazy val syssonDir = file(sys.props("user.home")) / "Desktop" / "IEM" / "SysSon"
  lazy val dataDir   = syssonDir / "netcdf" / "data"
  lazy val dataName  = "25_ta_Amon_HadGEM2-ES_rcp45_r1i1p1_200512-210012.nc"
  lazy val dataFile  = dataDir / "201211" / "gcm" / "RCP45" / "MetOffUK_HadGEM2-ES" / dataName
  lazy val recDir    = syssonDir / "rec"

  lazy val f = openFile(dataFile)

  def s = Server.default

  def record(name: String)(fun: => Synth) {
    val fRec  = recDir / s"${dataName}_$name.aif"
    val dfRec = SynthDef("_rec") {
      val sig = In.ar(0, 2)
      DiskOut.ar("buf".ir, Limiter.ar(sig, level = -0.2.dbamp))
    }
    val bRec  = Buffer(s)
    val syRec = Synth(s)

    print(s"Recording ${fRec.getName} ...")

    bRec.alloc(numFrames = 32768, numChannels = 2, completion =
      bRec.writeMsg(fRec.getAbsolutePath, sampleFormat = SampleFormat.Int16, numFrames = 0, startFrame = 0,
        leaveOpen = true, completion =
          dfRec.recvMsg(completion =
            syRec.newMsg(dfRec.name, addAction = addToTail, args = Seq("buf" -> bRec.id))
          )
      )
    )

    val sySon = fun
    sySon.onEnd {
      syRec.free()
      bRec.close()
      bRec.free()
      println(" Done.")
    }
  }
}
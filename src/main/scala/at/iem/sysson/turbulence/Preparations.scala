/*
 *  Preparations.scala
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

import de.sciss.file._
import de.sciss.fscape.FScapeJobs
import de.sciss.synth.io.AudioFile

object Preparations {
  def main(args: Array[String]): Unit = {
    args.headOption.getOrElse("???") match {
      case "--white-convs" => mkWhiteConvs()
      case other => println(s"Unsupported switch '$other'")
        sys.exit(1)
    }
  }

  def mkWhiteConvs(): Unit = {
    val fsc   = FScapeJobs()
    fsc.connect() { success =>
      println(if (success) "FScape connected." else "Failure - could not connect to FScape!")
    }

    val files = MakeLayers.Freesound.fileMap
    val dir   = Turbulence.audioWork / "conv"
    dir.mkdirs()

    import FScapeJobs._

    val jobs = files.toList.sortBy(_._1).flatMap { case (num, f) =>
      // println(s"Processing spk. $num...")
      val cut1  = "/tmp/fsc1.aif"
      val cut2  = "/tmp/fsc2.aif"
      val spec  = AudioFile.readSpec(f)
      val dur   = spec.numFrames / spec.sampleRate
      val dur1  = math.min(dur, 60.0)
      val off2  = dur - dur1
      val job1  = UnaryOp(in = f.path, out = cut1, offset = "0s", length = s"${dur1}s")
      val job2  = UnaryOp(in = f.path, out = cut2, offset = s"${off2}s", length = s"${dur1}s")
      val bl1   = "/tmp/fsc1b.aif"
      val bl2   = "/tmp/fsc2b.aif"
      val job3  = Bleach(in = cut1, out = bl1, twoWays = true)
      val job4  = Bleach(in = cut2, out = bl2, twoWays = true)
      val cv    = "/tmp/fscc.aif"
      val job5  = Convolution(in = bl1, impIn = bl2, out = cv, gain = Gain.normalized)
      val cvLen = dur1 * 2
      val fdLen = 8.0
      val cvLen1 = cvLen - fdLen
      val lpLen = math.min(60.0, cvLen1)
      val skip  = (cvLen - lpLen)/2 + fdLen
      val trunc = math.max(0.0, (cvLen1 - lpLen) - skip)
      val lp    = dir / s"fsm${num}conv.aif"
      val job6  = MakeLoop(in = cv, out = lp.path, length = s"${fdLen}s",
        offset = s"${skip}s", trunc = s"${trunc}s", pos = "pre",
        gain = Gain.normalized, spec = OutputSpec.aiffInt)
      job1 :: job2 :: job3 :: job4 :: job5 :: job6 :: Nil
    }

    println("Processing...")
    val sync = new AnyRef
    new Thread {
      override def run() = sync.synchronized(sync.wait())
      start()
    }

    fsc.processChain("mkWhiteConvs", jobs.toSeq, println) { success =>
      println(if (success) "Done." else "Failure!")
      sync.synchronized(sync.notifyAll())
      sys.exit(if (success) 0 else 1)
    }
  }
}
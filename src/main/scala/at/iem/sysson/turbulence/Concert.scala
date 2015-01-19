package at.iem.sysson.turbulence

import com.alee.laf.WebLookAndFeel

import de.sciss.file._
import de.sciss.lucre.stm
import de.sciss.lucre.swing.defer
import de.sciss.lucre.synth.InMemory
import de.sciss.nuages._
import de.sciss.synth
import de.sciss.synth.ugen.{Constant, LinXFade2}
import de.sciss.synth.{GE, Server}
import de.sciss.synth.proc.AuralSystem

object Concert extends App {
  implicit val system = InMemory()
  defer(WebLookAndFeel.install())
  (new Concert).run()
}
class Concert extends Wolkenpumpe[InMemory] {
  type S = InMemory
  private val dsl = new Wolkenpumpe.DSL[S]
  import dsl._

  override protected def configure(sCfg: ScissProcs.ConfigBuilder, nCfg: Nuages.ConfigBuilder,
                                   aCfg: Server.ConfigBuilder): Unit = {
    super.configure(sCfg, nCfg, aCfg)
    sCfg.generatorChannels  = 4 // ?
    sCfg.micInputs          = Vector(
      NamedBusConfig("m-dpa"  ,  2, 1),
      NamedBusConfig("m-at "  ,  0, 2)
    )
    sCfg.lineInputs         = Vector.empty
    sCfg.lineOutputs        = Vector(
      NamedBusConfig("sum", 44, 2)
    )
    sCfg.highPass           = 100
    sCfg.audioFilesFolder   = Some(userHome / "Music" / "tapes")

    // println(s"master max = ${Turbulence.ChannelIndices.max}")
    nCfg.masterChannels     = Some(Turbulence.ChannelIndices)
    nCfg.soloChannels       = Some(0 to 1)
    nCfg.recordPath         = Some("/tmp")

    aCfg.wireBuffers        = 512 // 1024
    aCfg.audioBuffers       = 4096
    aCfg.blockSize          = 128
  }

  private def mix(in: GE, flt: GE, mix: GE): GE = LinXFade2.ar(in, flt, mix * 2 - 1)
  private def mkMix()(implicit tx: S#Tx): GE = pAudio("mix", ParamSpec(0, 1), default = 0)

  private def mkMix4()(implicit tx: S#Tx): GE = {
    import synth._; import ugen._
    val f1 = pAudio("mix1", ParamSpec(0, 1), default = 0)
    val f2 = pAudio("mix2", ParamSpec(0, 1), default = 0)
    Lag.ar(Seq(f1, f1 * 0.667 + f2 * 0.333, f1 * 0.333, f2 * 0.667, f2))
  }

  private def mkTransition(name: String)(fun: (GE, GE) => GE)(implicit tx: S#Tx, nuages: Nuages[S]) = filter(name) { in =>
    import de.sciss.synth._
    import de.sciss.synth.ugen._
    val fade   = mkMix()
    val sig   = fun(in, 1 - fade)
    sig // mix(in, sig, fade)
  }

  // a 10% direct fade-in/out, possibly with delay to compensate for FFT
  private def mkBlend(pred: GE, z: GE, fade: GE, dt: GE = Constant(0)): GE = {
    import de.sciss.synth._
    import de.sciss.synth.ugen._
    val dpa = fade.min(0.1) * 10
    val pa = fade.min(0.1).linlin(0, 0.1, 1, 0)
    val za = 1 - pa
    val dp = if (dt == Constant(0)) pred else DelayN.ar(pred, dt, dt * dpa)
    val pm = dp * pa
    val zm = z  * za
    pm + zm
  }

  override protected def registerProcesses(sCfg: ScissProcs.Config, nCfg: Nuages.Config)
                                          (implicit tx: S#Tx, cursor: stm.Cursor[InMemory],
                                           nuages: Nuages[S], aural: AuralSystem): Unit = {
    super.registerProcesses(sCfg, nCfg)

    filter("L-lpf") { in =>
      import de.sciss.synth._
      import de.sciss.synth.ugen._
      val fade  = mkMix4()
      val freq  = fade.linexp(1, 0, 22.05 * 2, 20000) // 22050
      val wet   = LPF.ar(in, freq)
      mkBlend(in, wet, fade)
    }

    filter("L-hpf") { in =>
      import synth._; import ugen._
      val fade  = mkMix4()
      val freq  = fade.linexp(1, 0, 20000, 22.05 * 2)
      val wet   = HPF.ar(HPF.ar(in, freq), freq)
      mkBlend(in, wet, fade)
    }

    val FFTSize = 512

    filter("L-below") { in =>
      import synth._; import ugen._
      val fade    = mkMix4()
      val thresh  = fade.linexp(1, 0, 1.0e-3, 1.0e1)
      val buf     = LocalBuf(FFTSize)
      val wet     = IFFT.ar(PV_MagBelow(FFT(buf, in), thresh))
      mkBlend(in, wet, fade, FFTSize / SampleRate.ir)
    }

    filter("L-above") { in =>
      import synth._; import ugen._
      val fade    = mkMix4()
      val thresh  = fade.linexp(0, 1, 1.0e-3, 2.0e1)
      val buf     = LocalBuf(FFTSize)
      val wet     = IFFT.ar(PV_MagAbove(FFT(buf, in), thresh))
      mkBlend(in, wet, fade, FFTSize / SampleRate.ir)
    }

    filter("L-up") { in =>
      import synth._; import ugen._
      val fade    = mkMix4()
      val numSteps = 16 // 10
      val x        = (1 - fade) * numSteps
      val xh       = x / 2
      val a        = (xh + 0.5).floor        * 2
      val b0       = (xh       .floor + 0.5) * 2
      val b        = b0.min(numSteps)
      val ny       = 20000 // 22050
      val zero     = 22.05
      val aFreq    = a.linexp(numSteps, 0, zero, ny) - zero
      val bFreq    = b.linexp(numSteps, 0, zero, ny) - zero
      val freq: GE = Seq(aFreq, bFreq)

      val z0      = FreqShift.ar(LPF.ar(in, ny - freq),  freq)

      val zig     = x.fold(0, 1)
      val az      = zig     // .sqrt
      val bz      = 1 - zig // .sqrt
      val wet     = az * (z0 \ 1 /* aka ceil */) + bz * (z0 \ 0 /* aka floor */)

      mkBlend(in, wet, fade)
    }

    filter("L-down") { in =>
      import synth._; import ugen._
      val fade    = mkMix4()
      val numSteps = 16
      val x        = (1 - fade) * numSteps
      val xh       = x / 2
      val a        = (xh + 0.5).floor        * 2
      val b0       = (xh       .floor + 0.5) * 2
      val b        = b0.min(numSteps)
      val fd: GE   = Seq(a, b)
      val ny       = 20000 // 20000 // 22050
      val zero     = 22.05
      val freq1    = fd.linexp(0, numSteps, ny, zero)
      val freq2    = fd.linexp(0, numSteps, zero, ny) - zero

      val fltSucc   = HPF.ar(in, freq1)
      val z0        = FreqShift.ar(fltSucc, -freq1)

      val zig = x.fold(0, 1)
      val az  = zig      // .sqrt
      val bz  = 1 - zig  // .sqrt
      val wet = az * (z0 \ 1 /* aka ceil */) + bz * (z0 \ 0 /* aka floor */)

      mkBlend(in, wet, fade)
    }

    /*
    collector("vbap") { in =>
      import synth._; import ugen._
      def calcIndices(xf: GE, yf: GE): ((GE, GE, GE), (GE, GE, GE), (GE, GE, GE)) = {
        val x   = xf.floor
        val u   = xf - x
        val y0  = yf.floor
        val v   = yf - y0

        // note, we add two to avoid problems with negative numbers,
        // but have to take care to subtract that later
        val y   = {
          val cond1 = (y0 % 2) sig_!= (x % 2)
          val cond2 = u + v < 1
          val cond3 = u - v > 0
          val cond  = (cond1 & cond2) | cond3
          y0 + 2 - cond  // if (cond) y0 + 1 else y0 + 2
        }

        val yDiv  = (y / 2).floor - 1 // the minus 1 corrects the offset
        val yOdd  = y % 2
        val yEven = 1 - yOdd
        val xOdd  = x % 2

        val vx1  = x + yOdd
        val vy1i = yDiv + yOdd
        val vy1  = vy1i * 2 + (vx1 % 2)

        val vx2  = x + (yOdd ^ xOdd)
        val vy2i = yDiv + yEven
        val vy2  = vy2i * 2 + (vx2 % 2)

        val vx3  = x + yEven
        val vy3i = yDiv + yOdd
        val vy3  = vy3i * 2 + (vx3 % 2)

        // cf. https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
        def dist(x: GE, y: GE)(lx1: GE, ly1: GE, lx2: GE, ly2: GE): GE = {
          // |Dy x0 - Dx y0 - x1 y2 + x2 y1| / sqrt(Dx.squared + Dy.squared)
          // - we assume a normalized Dx, Dy, and thus drop the division
          // - indeed we know the height is two and divide by it

          val dx = lx2 - lx1
          val dy = ly2 - ly1
          //  (dy * x - dx * y - lx1 * ly2 + lx2 * ly1).abs / 2
          Sum4(dy * x, -dx * y, -lx1 * ly2, lx2 * ly1).abs / 2
        }

        val df = dist(xf, yf) _
        val d1 = df(vx2, vy2, vx3, vy3)
        val d2 = df(vx3, vy3, vx1, vy1)
        val d3 = df(vx1, vy1, vx2, vy2)
        // println(f"d1 = $d1%1.2f, d2 = $d2%1.2f, d3 = $d3%1.2f, sum = ${d1 + d2 + d3}%1.2f")
        val g1 = d1.sqrt
        val g2 = d2.sqrt
        val g3 = d3.sqrt

        ((vx1, vy1i, g1), (vx2, vy2i, g2), (vx3, vy3i, g3))
      }

      ...
    }
    */
  }
}
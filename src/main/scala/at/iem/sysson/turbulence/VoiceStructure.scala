/*
 *  VoiceStructure.scala
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
import de.sciss.lucre.expr.{Expr, Boolean => BooleanEx, Int => IntEx, String => StringEx}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike
import de.sciss.mellite.Workspace
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{StringElem, ObjKeys, SoundProcesses, BooleanElem, Folder, IntElem, Action, Code, Ensemble, ExprImplicits, Obj, Proc, Scan, graph}
import de.sciss.synth.{GE, SynthGraph}
import ucar.nc2.time.{CalendarPeriod, CalendarDateFormatter}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.Future
import scala.concurrent.stm.Ref

object VoiceStructure {
  import MakeWorkspace.DEBUG

  val DumpOSC         = false
  val ShowLog         = false
  var ShowNodeTree    = false   // warning - buggy
  val PrintStates     = false
  val Shadowing       = true

  val Attack          = 30  // 10
  val Release         = 30  // 10
  val FFTSize         = 512 // 1024

  lazy val NumLayers      = if (DEBUG) 2 else Turbulence.NumWiredSensors + 1  // one for each sensor plus bg
  lazy val MaxVoices      = if (DEBUG) 1 else 3
  lazy val NumChannels    = /* if (DEBUG) 2 else */ Turbulence.NumChannels
  lazy val NumTransitions = if (DEBUG) 1 else 7

  // (1)
  def apply[S <: Sys[S]]()(implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: Workspace[S],
                           compiler: Code.Compiler): Future[Unit] = {
    val vs        = new VoiceStructure[S]
    val doneFut   = vs.mkDoneAction()
    val timeFut   = vs.mkTimeAction()
    import Folder.serializer
    import SoundProcesses.executionContext
    Future.sequence(Seq(doneFut, timeFut)).map { case Seq(doneH, timeH) =>
      cursor.step { implicit tx =>
        val all = vs.mkWorld(done = doneH(), time = timeH())
        workspace.root.addHead(all)
        // tx.newHandle(all)
      }
    }
  }

  implicit class ScanOps[S <: Sys[S]](val `this`: Scan[S]) extends AnyVal {
    def ~> (that: Scan[S])(implicit tx: S#Tx): Unit =
      `this`.addSink(Scan.Link.Scan(that))

    def ~/> (that: Scan[S])(implicit tx: S#Tx): Unit =
      `this`.removeSink(Scan.Link.Scan(that))
  }

  def rrand(lo: Int, hi: Int)(implicit rnd: util.Random): Int = rnd.nextInt(hi - lo + 1) + lo

  def doneAction[S <: Sys[S]](self: Action.Obj[S], root: Folder[S])(implicit tx: S#Tx): Unit = {
    val imp = ExprImplicits[S]
    import imp._
    import MakeWorkspace.DEBUG

    for {
      Expr.Var(state) <- self.attr[IntElem]("state")
    } {
      val old = state.value
      if (old == 2) {        // fade-in  -> engaged
        state() = 1
      } else if (old == 3) { // fade-out -> bypass
        state() = 0
        val stop = self.attr[IntElem]("active").exists(_.value == 0)
        if (stop) {
          val li = self.attr[IntElem]("li").get.value
          if (DEBUG) println(s"Layer $li stopping")
          for {
            Ensemble.Obj(layers) <- root   / "layers"
            Ensemble.Obj(lObj)   <- layers / s"layer-$li"
            Proc.Obj(pred)       <- lObj   / s"pred$li"
            Proc.Obj(out )       <- lObj   / s"foo$li"
          } {
            if (DEBUG) println(s"Unlink (${pred.name}, ${out.name})")
            (pred, out).unlink()
            lObj.stop()
          }
        }
      }
    }
  }

  private val date1850 = CalendarDateFormatter.isoStringToCalendarDate(null, "1850-01-01 00:00:00")

  private val timeFramesSince1850 = Ref(0)

  private final val daysPerMonth = 365.2422 / 12  // cf. http://pumas.jpl.nasa.gov/examples/index.php?id=46

  def CurrentFrame1850(implicit tx: TxnLike): Int = timeFramesSince1850.get(tx.peer)

  def timeAction[S <: Sys[S]](self: Action.Obj[S], root: Folder[S], values: Vec[Float])(implicit tx: S#Tx): Unit = {
    val imp = ExprImplicits[S]
    import imp._

    val days    = values(0)
    val frames  = (days / daysPerMonth).toInt
    timeFramesSince1850.set(frames)(tx.peer)
    val date    = date1850.add(days, CalendarPeriod.Field.Day)
    val s       = CalendarDateFormatter.toDateTimeString(date)
    val li      = self.attr[IntElem]("li").map(_.value).getOrElse(-1)
    val varName = if (li < 0) "???" else MakeLayers.all(li).varName

    Report.send[S](li = li, varName = varName, date = s)
    // println(s"TIME = $s; FRAMES = $frames")
  }
}
class VoiceStructure[S <: Sys[S]] {
  import VoiceStructure._
  import MakeWorkspace.DEBUG

  private[this] val imp = ExprImplicits[S]
  import imp._

  def mkDoneAction()(implicit tx: S#Tx, cursor: stm.Cursor[S],
                     compiler: Code.Compiler): Future[stm.Source[S#Tx, Action[S]]] = {
    println("Making Voices Done Action")
    val source =
      """import at.iem.sysson.turbulence._
        |VoiceStructure.doneAction(self, root)
        |""".stripMargin
    val code  = Code.Action(source)
    Action.compile[S](code)
  }

  def mkTimeAction()(implicit tx: S#Tx, cursor: stm.Cursor[S],
                 compiler: Code.Compiler): Future[stm.Source[S#Tx, Action[S]]] = {
    println("Making Voices Time Action")
    val source =
      """import at.iem.sysson.turbulence._
        |VoiceStructure.timeAction(self, root, values)
        |""".stripMargin
    val code  = Code.Action(source)
    Action.compile[S](code)
  }

  // a 10% direct fade-in/out, possibly with delay to compensate for FFT
  private def mkBlend(pred: GE, succ: GE, z: GE, fade: GE, dt: GE): GE = {
    import de.sciss.synth._
    import de.sciss.synth.ugen._
    val dpa = fade.min(0.1) * 10
    val pa = 1 - dpa
    val sa = (fade - 0.9).max(0.0) * 10
    val dsa = 1 - sa
    val za = 1 - (pa max sa)
    // val pm = pred * pa
    // val sm = succ * sa
    val dp = if (dt == Constant(0)) pred else DelayN.ar(pred, dt, dt * dpa)
    val sp = if (dt == Constant(0)) succ else DelayN.ar(succ, dt, dt * dsa)
    val pm = dp * pa
    val sm = sp * sa
    val zm = z    * za
    //       pa.poll(1, "pa")
    //       sa.poll(1, "sa")
    //       za.poll(1, "za")
    pm + sm + zm
  }

  private def mkTransition(fun: (GE, GE, GE) => GE): SynthGraph = SynthGraph {
    import de.sciss.synth._
    import de.sciss.synth.ugen._
    val pred  = graph.ScanInFix("pred", 1)
    val succ  = graph.ScanInFix("succ", 1)
    val state = graph.Attribute.kr("state", 2)
    val target = 3 - state // 1 for fade-in, 0 for fade-out
    val start  = 1 - target
    val atk    = Attack
    val rls    = Release
    val in     = Select.kr(ToggleFF.kr(1), Seq(start, target))
    val fade   = Slew.kr(in, atk.reciprocal, rls.reciprocal)
    if (DEBUG) fade.poll(1, "fade")
    val done   = fade sig_== target
    graph.Action(done, "done")
    val sig   = fun(pred, succ, fade)
    graph.ScanOut(sig)
  }

  private lazy val transGraphs: Vec[SynthGraph] = {
    // transition 1: rising LPF
    val t1 = mkTransition { (pred, succ, fade) =>
      import de.sciss.synth._
      import de.sciss.synth.ugen._
      val freq = fade.linexp(0, 1, 22.05, 22050)
      HPF.ar(pred, freq) + LPF.ar(succ, freq)
    }

    // transition 2: descending HPF
    val t2 = mkTransition { (pred, succ, fade) =>
      import de.sciss.synth._
      import de.sciss.synth.ugen._
      val freq = fade.linexp(1, 0, 22.05, 22050)
      HPF.ar(succ, freq) + LPF.ar(pred, freq)
    }

    // transition 3: rising PV_MagBelow
    val t3 = mkTransition { (pred, succ, fade) =>
      import de.sciss.synth._
      import de.sciss.synth.ugen._
      val thresh  = fade.linexp(0, 1, 1.0e-3, 1.0e1)
      val bufPred = LocalBuf(FFTSize)
      val bufSucc = LocalBuf(FFTSize)
      val fltPred = IFFT.ar(PV_MagAbove(FFT(bufPred, pred), thresh))
      val fltSucc = IFFT.ar(PV_MagBelow(FFT(bufSucc, succ), thresh))
      val z = fltPred + fltSucc
      mkBlend(pred, succ, z, fade, FFTSize / SampleRate.ir)
    }

    // transition 4: descending PV_MagAbove
    val t4 = mkTransition { (pred, succ, fade) =>
      import de.sciss.synth._
      import de.sciss.synth.ugen._
      val thresh  = fade.linexp(1, 0, 1.0e-3, 1.0e1)
      val bufPred = LocalBuf(FFTSize)
      val bufSucc = LocalBuf(FFTSize)
      val fltPred = IFFT.ar(PV_MagBelow(FFT(bufPred, pred), thresh))
      val fltSucc = IFFT.ar(PV_MagAbove(FFT(bufSucc, succ), thresh))
      val z = fltPred + fltSucc
      mkBlend(pred, succ, z, fade, FFTSize / SampleRate.ir)
    }

    // transition 5: to dust
    val t5 = mkTransition { (pred, succ, fade) =>
      import de.sciss.synth._
      import de.sciss.synth.ugen._
      val f1   =   10
      val f2   = 2000

      val dustFreqS = fade.linexp(0, 1, f1, f2)
      val dustFreqP = fade.linexp(1, 0, f1, f2)

      val decayTime = 0.01
      val dustS = Decay.ar(Dust.ar(dustFreqS), decayTime).min(1)
      val dustP = Decay.ar(Dust.ar(dustFreqP), decayTime).min(1)

      val fltSucc = succ * dustS
      val fltPred = pred * dustP

      //      val fadeIn = Line.kr(0, 1, dur = 2)
      //      val sig = in * (1 - fadeIn) + mod * fadeIn
      val z = fltSucc + fltPred

      mkBlend(pred, succ, z, fade, 0)
    }

    // transition 6: shift upwards
    val t6 = mkTransition { (pred, succ, fade) =>
      import de.sciss.synth._
      import de.sciss.synth.ugen._

      val numSteps = 16 // 10
      val x        = fade * numSteps
      val xh       = x / 2
      val a        = (xh + 0.5).floor        * 2
      val b        = (xh       .floor + 0.5) * 2
      val ny       = 20000 // 22050
      val aFreq    = a.linexp(numSteps, 0, 22.05, ny) - 22.05
      val bFreq    = b.linexp(numSteps, 0, 22.05, ny) - 22.05
      val freq: GE = Seq(aFreq, bFreq)

      val fltSucc = FreqShift.ar(LPF.ar(succ, ny - freq),  freq)
      val fltPred = FreqShift.ar(HPF.ar(pred, ny - freq), -freq)

      val z0  = fltSucc + fltPred
      val zig = x.fold(0, 1)
      val az  = zig     // .sqrt
      val bz  = 1 - zig // .sqrt
      val z   = az * (z0 \ 1 /* aka ceil */) + bz * (z0 \ 0 /* aka floor */)

      mkBlend(pred, succ, z, fade, 0)
    }

    // transition 7: shift downwards
    val t7 = mkTransition { (pred, succ, fade) =>
      import de.sciss.synth._
      import de.sciss.synth.ugen._

      val numSteps = 16
      val x        = fade * numSteps
      val xh       = x / 2
      val a        = (xh + 0.5).floor        * 2
      val b        = (xh       .floor + 0.5) * 2
      val fd: GE   = Seq(a, b)
      val ny       = 20000 // 22050
      val freq1    = fd.linexp(0, numSteps, ny, 22.05)
      val freq2    = fd.linexp(0, numSteps, 22.05, ny) - 22.05

      val fltSucc = FreqShift.ar(HPF.ar(succ, freq1), -freq1)
      val fltPred = FreqShift.ar(LPF.ar(pred, ny - freq2),  freq2)

      val z0  = fltSucc + fltPred
      val zig = x.fold(0, 1)
      val az  = zig       // .sqrt
      val bz  = (1 - zig) // .sqrt
      val z   = az * (z0 \ 1 /* aka ceil */) + bz * (z0 \ 0 /* aka floor */)

      mkBlend(pred, succ, z, fade, 0)
    }

    val transGraphs0  = Vec(t1, t2, t3, t4, t5, t6, t7)
    val res   = Vec.tabulate(NumTransitions)(i => transGraphs0(i % transGraphs0.size))
    // val res = Vec.fill(NumTransitions)(t1)
    assert(res.size == NumTransitions)
    res
  }

  import BooleanEx.{serializer => boolSer, varSerializer => boolVarSer}
  import IntEx    .{serializer => intSer , varSerializer => intVarSer }

  def mkWorld(done: Action[S], time: Action[S])(implicit tx: S#Tx, workspace: Workspace[S]): Ensemble.Obj[S] = {
    println("Making Voices World")
    //    val sensors = Vec.tabulate(NumSpeakers) { speaker =>
    //      val sensor = IntEx.newVar[S](-1)
    //      if (PrintStates) sensor.changed.react(_ => ch => println(s"sensor$speaker -> ${ch.now}"))
    //      sensor
    //    }

    val diff = Proc[S]
    diff.graph() = SynthGraph {
      import de.sciss.synth._
      import de.sciss.synth.ugen._
      val in = graph.ScanInFix(NumChannels)
      Turbulence.ChannelIndices.zipWithIndex.foreach { case (bus, ch) =>
        val inc   = in \ ch
        val check = CheckBadValues.ar(inc, ch)
        val ok    = Gate.ar(inc, check sig_== 0)
        val flt   = HPF.ar(ok, 110.25)  // make Georgios happy, and perhaps the amplifier
        Out.ar(bus, flt)
      }

//      val mix = Mix.tabulate(NumChannels) { ch =>
//        val inc = in \ ch
//        val pan = if (NumChannels == 1) 0.0 else ch.linlin(0, NumChannels - 1, -1, 1)
//        val sig = Pan2.ar(inc, pan)
//        sig
//      }
//      Out.ar(0, mix)
    }
    diff.scans.add("in")
    val diffObj = Obj(Proc.Elem(diff))
    diffObj.name = "diff"

    val vecLayer  = MakeLayers.all.zipWithIndex.map { case (factory, li) =>
      mkLayer(diff = diff, done = done, time = time, li = li, factory = factory)
    }

    val vecPlaying      = vecLayer.map(_.elem.peer.playing)
    val activeVoices    = count(vecPlaying)
    val hasFreeVoices   = activeVoices < MaxVoices
    if (PrintStates) activeVoices.changed.react(_ => ch => println(s"activeVoices -> ${ch.now}"))

    // val wTransId = IntEx.newVar[S](0)

    val allFolder = Folder[S]
    val all       = Ensemble(allFolder, 0L, BooleanEx.newVar[S](true))
    allFolder.addLast(diffObj)
    vecLayer.foreach(ensLObj => allFolder.addLast(ensLObj))
    val allObj    = Obj(Ensemble.Elem(all))
    //    sensors.zipWithIndex.foreach { case (s, si) =>
    //      allObj.attr.put(f"s$si%02d", Obj(IntElem(s)))
    //    }
    // allObj.attr.put("trans", Obj(IntElem(wTransId)))
    allObj.attr.put("free-voices", Obj(BooleanElem(hasFreeVoices)))
    allObj.name = "layers"
    allObj
  }

  // for simplicity, same graph for
  // all layers, distinguished by
  // resonant frequency depending on
  // attribute 0 <= `li` < NumLayers
  private lazy val genGraph = SynthGraph {
    import de.sciss.synth._
    import de.sciss.synth.ugen._
    val li    = graph.Attribute.ir("li", 0)
    val freq  = if (NumLayers == 1) 1000.0: GE else li.linexp(0, NumLayers - 1, 200.0, 4000.0)
    val amp   = 0.5
    val dust  = Decay.ar(Dust.ar(Seq.fill(NumChannels)(10)), 1).min(1)
    val sig   = Resonz.ar(dust, freq, 0.5) * amp
    graph.ScanOut(sig)
  }

  // multi-channel single scan in, multiple signal-channel scan outs
  private lazy val splitGraph = SynthGraph {
    val in = graph.ScanInFix(NumChannels)
    Vec.tabulate(NumChannels) { ch =>
      graph.ScanOut(s"out$ch", in \ ch)
    }
  }

  // multiple signal-channel scan ins, multi-channel single scan out,
  private lazy val collGraph = SynthGraph {
    import de.sciss.synth.ugen._
    val in = Vec.tabulate(NumChannels) { ch =>
      graph.ScanInFix(s"in$ch", 1)
    }
    graph.ScanOut(Flatten(in))
  }

  // simply in -> out
  private lazy val throughGraph = SynthGraph {
    graph.ScanOut(graph.ScanIn())
  }

  private lazy val switchGraph = SynthGraph {
    import de.sciss.synth.ugen._
    val pred    = graph.ScanInFix("pred", 1)
    val succ    = graph.ScanInFix("succ", 1)
    val state   = graph.Attribute.kr("state", 2)  // 0 - bypass (pred), 1 - engage (succ)
    val sig     = Select.ar(state, Seq(pred, succ))
    graph.ScanOut(sig)
  }

  private def mkLayer(diff: Proc[S], done: Action[S], time: Action[S], li: Int,
                      factory: LayerFactory)
                     (implicit tx: S#Tx, workspace: Workspace[S]): Ensemble.Obj[S] = {
    val transId     = IntEx.newVar[S](0) // "sampled" in `checkWorld`
    val transIdObj  = Obj(IntElem(transId))

    // layer-level ensemble
    val lFolder     = Folder[S]

    // the actual sound layer
    val (genOuterObj, genProcObj) = factory.mkLayer()
    //    val gen       = Proc[S]
    //    gen.graph()   = genGraph
    //    val genObj    = Obj(Proc.Elem(gen))
    val liObj       = Obj(IntElem(li))
    genOuterObj.attr.put("li", liObj)
    genOuterObj.name = s"gen$li"
    val timeObj     = Obj(Action.Elem(time))
    timeObj.attr.put("li"   , liObj   )
    genProcObj.attr.put("time", timeObj)
    lFolder.addLast(genOuterObj)

    // layer-ensemble input from predecessor
    val pred        = Proc[S]
    pred.graph()    = throughGraph
    pred.scans.add("in")
    val predObj     = Obj(Proc.Elem(pred))
    predObj.name = s"pred$li"
    lFolder.addLast(predObj)

    // aka background splitter
    val split       = Proc[S]
    split.graph()   = splitGraph
    val splitObj    = Obj(Proc.Elem(split))
    splitObj.name   = s"split$li"
    lFolder.addLast(splitObj)
    pred.scans.add("out") ~> split.scans.add("in")

    // aka foreground splitter
    val succ        = Proc[S]
    succ.graph()    = splitGraph
    val succObj     = Obj(Proc.Elem(succ))
    succObj.name    = s"succ$li"
    lFolder.addLast(succObj)
    val genProc     = genProcObj.elem.peer
    genProc.scans.add("out") ~> succ.scans.add("in")

    // aka collector
    val coll        = Proc[S]
    coll.graph()    = collGraph
    val collObj     = Obj(Proc.Elem(coll))
    collObj.name    = s"coll$li"
    lFolder.addLast(collObj)

    // layer-ensemble output to successor
    val out         = Proc[S]
    out.graph()     = throughGraph
    out.scans.add("out")
    val outObj      = Obj(Proc.Elem(out))
    outObj.name     = s"foo$li"
    lFolder.addLast(outObj)
    coll.scans.add("out") ~> out.scans.add("in")

    class Channel(val stateObj: Obj[S], val state: Expr.Var[S, Int], val fPlaying: Expr[S, Boolean],
                  val active: Expr[S, Boolean], val predOut: Scan[S], val succOut: Scan[S], val collIn: Scan[S],
                  val doneObj: Action.Obj[S])

    val vecChannels = Vec.tabulate[Channel](NumChannels) { si =>
      val state     = IntEx.newVar[S](0)  // 0 - bypass, 1 - engaged, 2 - fade-in, 3 - fade-out
      val stateObj  = Obj(IntElem(state))
      if (PrintStates) state.changed.react(_ => ch => println(s"state${li}_$si -> ${ch.now}"))
      val fPlaying  = state >= 2 // ongoing transition per channel
      if (PrintStates) fPlaying.changed.react(_ => ch => println(s"fPlaying${li}_$si -> ${ch.now}"))

      val predOut   = split .scans.add(s"out$si")
      val succOut   = succ  .scans.add(s"out$si")
      val collIn    = coll  .scans.add(s"in$si")

      val procB     = Proc[S]   // transition bypass/engage per channel
      procB.graph() = switchGraph
      val procBObj  = Obj(Proc.Elem(procB))
      procBObj.attr.put("state", stateObj)
      val name = Obj(StringElem(StringEx.newVar[S](s"by${li}_$si")))
      procBObj.attr.put(ObjKeys.attrName, name)
      val bPlaying  = state <  2
      val bFolder   = Folder[S]
      bFolder.addLast(procBObj)
      val ensB      = Ensemble(bFolder, 0L, bPlaying)
      val ensBObj   = Obj(Ensemble.Elem(ensB))
      ensBObj.attr.put(ObjKeys.attrName, name)
      val predInB   = procB .scans.add("pred")
      val succInB   = procB .scans.add("succ")
      val outB      = procB .scans.add("out")
      lFolder.addLast(ensBObj)
      predOut ~> predInB
      succOut ~> succInB
      outB    ~> collIn

      val active    = state > 0

      val doneObj   = Obj(Action.Elem(done))
      doneObj.attr.put("state", stateObj)
      doneObj.attr.put("li"   , liObj   )

      new Channel(stateObj = stateObj, state = state, fPlaying = fPlaying, active = active,
        predOut = predOut, succOut = succOut, collIn = collIn, doneObj = doneObj)
    }

    val activeCount = count(vecChannels.map(_.active))
    if (PrintStates) activeCount.changed.react(_ => ch => println(s"activeCount$li -> ${ch.now}"))
    val activeCountObj = Obj(IntElem(activeCount))
    vecChannels.foreach { ch => ch.doneObj.attr.put("active", activeCountObj) }

    val lPlaying    = BooleanEx.newVar[S](false)
    // if (PrintStates) lPlaying.changed.react(_ => ch => println(s"lPlaying$li -> ${ch.now}"))

    val ensL    = Ensemble[S](lFolder, 0L, lPlaying)
    val ensLObj = Obj(Ensemble.Elem(ensL))
    ensLObj.name = s"layer-$li"
    ensLObj.attr.put("trans", transIdObj)

    transGraphs.zipWithIndex.foreach { case (g, gi) =>
      val tPlaying    = transId sig_== gi
      val tFolder     = Folder[S]
      val ensT        = Ensemble[S](tFolder, 0L, tPlaying)
      val ensTObj     = Obj(Ensemble.Elem(ensT))
      ensTObj.name = s"trans-$gi"
      lFolder.addLast(ensTObj)

      vecChannels.zipWithIndex.foreach { case (channel, si) =>
        val fFolder   = Folder[S]
        val ensF      = Ensemble(fFolder, 0L, channel.fPlaying)
        val ensFObj   = Obj(Ensemble.Elem(ensF))
        val name      = Obj(StringElem(StringEx.newVar[S](s"t${gi}_$si")))
        ensFObj.attr.put(ObjKeys.attrName, name)
        tFolder.addLast(ensFObj)

        val procT     = Proc[S]
        procT.graph() = g
        val predInT   = procT.scans.add("pred")
        val succInT   = procT.scans.add("succ")
        val outT      = procT.scans.add("out")

        channel.predOut ~> predInT
        channel.succOut ~> succInT
        outT            ~> channel.collIn

        val procTObj  = Obj(Proc.Elem(procT))
        val attr      = procTObj.attr
        attr.put(ObjKeys.attrName, name)
        attr.put("state", channel.stateObj)
        attr.put("done" , channel.doneObj )

        fFolder.addLast(procTObj)
      }
    }

    // short debug solution; just connect all layer outputs to main diffusion
    if (!Shadowing) coll.scans.add("out") ~> diff.scans.add("in")

    ensLObj
  }

  private def count(in: Vec[Expr[S, Boolean]])(implicit tx: S#Tx): Expr[S, Int] = {
    val imp = ExprImplicits[S]
    import imp._
    val res = reduce(in.map(_.toInt))(_ + _)
    IntEx.newVar(res)   // helps decreasing serialization space usage :-E
  }

  // like Vec.reduce, but splitting at the half,
  // thus allowing the composition of bin-ops with guaranteed
  // tree depth of ld(N)
  private def reduce[A](in: Vec[A])(op: (A, A) => A): A = {
    val sz = in.size
    if (sz <= 1) in.head else {
      val (front, back) = in.splitAt(sz >> 1)
      val a = reduce(front)(op)
      val b = reduce(back )(op)
      op(a, b)
    }
  }
}
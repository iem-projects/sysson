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
import de.sciss.mellite.Workspace
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{StringElem, ObjKeys, SoundProcesses, BooleanElem, Folder, IntElem, Action, Code, Ensemble, ExprImplicits, Obj, Proc, Scan, graph}
import de.sciss.synth.{GE, SynthGraph}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.Future

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

  lazy val NumLayers      = if (DEBUG) 2 else Turbulence.NumWiredSensors
  lazy val MaxVoices      = if (DEBUG) 1 else 3
  lazy val NumChannels    = /* if (DEBUG) 2 else */ Turbulence.NumChannels
  lazy val NumTransitions = if (DEBUG) 1 else 7

  // (1)
  def apply[S <: Sys[S]]()(implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: Workspace[S],
                           compiler: Code.Compiler): Future[Unit] = {
    val vs        = new VoiceStructure[S]
    val actionFut = vs.mkAction()
    import Folder.serializer
    import SoundProcesses.executionContext
    actionFut.map { actionH =>
      cursor.step { implicit tx =>
        val all = vs.mkWorld(actionH())
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

  def rrand(lo: Int, hi: Int): Int = util.Random.nextInt(hi - lo + 1) + lo
}
class VoiceStructure[S <: Sys[S]] {
  import VoiceStructure._
  import MakeWorkspace.DEBUG

  private[this] val imp = ExprImplicits[S]
  import imp._

  def mkAction()(implicit tx: S#Tx, cursor: stm.Cursor[S],
                 compiler: Code.Compiler): Future[stm.Source[S#Tx, Action[S]]] = {
    println("Making Voices Action")
    val source =
      """val imp = ExprImplicits[S]
        |import imp._
        |import at.iem.sysson.turbulence._
        |import MakeWorkspace.DEBUG
        |
        |for {
        |  Expr.Var(state) <- self.attr[IntElem]("state")
        |} {
        |  val old = state.value
        |  if (old == 2) {        // fade-in  -> engaged
        |    state() = 1
        |  } else if (old == 3) { // fade-out -> bypass
        |    state() = 0
        |    val stop = self.attr[IntElem]("active").exists(_.value == 0)
        |    if (stop) {
        |      val li = self.attr[IntElem]("li").get.value
        |      if (DEBUG) println(s"Layer $li stopping")
        |      for {
        |        Ensemble.Obj(layers) <- root   / "layers"
        |        Ensemble.Obj(lObj)   <- layers / s"layer-$li"
        |        Proc.Obj(pred)       <- lObj   / s"pred$li"
        |        Proc.Obj(out )       <- lObj   / s"foo$li"
        |      } {
        |        if (DEBUG) println(s"Unlink (${pred.name}, ${out.name})")
        |        (pred, out).unlink()
        |        lObj.stop()
        |      }
        |    }
        |  }
        |}
        |""".stripMargin
    val code  = Code.Action(source)
    Action.compile[S](code)
  }

  private lazy val transGraphs: Vec[SynthGraph] = {
    def mkTransition(fun: (GE, GE, GE) => GE): SynthGraph = SynthGraph {
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
      fltSucc + fltPred
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
      fltSucc + fltPred
    }

    // transition 5: to dust
    val t5 = mkTransition { (pred, succ, fade) =>
      import de.sciss.synth._
      import de.sciss.synth.ugen._
      val f1   =   20
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

      fltSucc + fltPred
    }

    // transition 6: shift upwards
    val t6 = mkTransition { (pred, succ, fade) =>
      import de.sciss.synth._
      import de.sciss.synth.ugen._

      val freq = fade.linexp(1, 0, 22.05, 22050) - 22.05

      val fltSucc = FreqShift.ar(LPF.ar(succ, 22050 - freq),  freq)
      val fltPred = FreqShift.ar(HPF.ar(pred, 22050 - freq), -freq)

      fltSucc + fltPred
    }

    // transition 7: shift downwards
    val t7 = mkTransition { (pred, succ, fade) =>
      import de.sciss.synth._
      import de.sciss.synth.ugen._

      val freq = fade.linexp(0, 1, 22.05, 22050) - 22.05

      val fltSucc = FreqShift.ar(HPF.ar(succ, 22050 - freq), -freq)
      val fltPred = FreqShift.ar(LPF.ar(pred, 22050 - freq),  freq)

      fltSucc + fltPred
    }

    val transGraphs0  = Vec(t1, t2, t3, t4, t5, t6, t7)
    val res   = Vec.tabulate(NumTransitions)(i => transGraphs0(i % transGraphs0.size))
    // val res = Vec.fill(NumTransitions)(t1)
    assert(res.size == NumTransitions)
    res
  }

  import BooleanEx.{serializer => boolSer, varSerializer => boolVarSer}
  import IntEx    .{serializer => intSer , varSerializer => intVarSer }

  def mkWorld(done: Action[S])(implicit tx: S#Tx, workspace: Workspace[S]): Ensemble.Obj[S] = {
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
        val inc = in \ ch
        val flt = HPF.ar(inc, 110.25)  // make Georgios happy, and perhaps the amplifier
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
      mkLayer(diff, done, li, factory)
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

  private def mkLayer(diff: Proc[S], done: Action[S], li: Int,
                      factory: LayerFactory)
                     (implicit tx: S#Tx, workspace: Workspace[S]): Ensemble.Obj[S] = {
    val transId     = IntEx.newVar[S](0) // "sampled" in `checkWorld`
    val transIdObj  = Obj(IntElem(transId))

    // layer-level ensemble
    val lFolder = Folder[S]

    // the actual sound layer
    val (genObj, gen) = factory.mkLayer()
    //    val gen       = Proc[S]
    //    gen.graph()   = genGraph
    //    val genObj    = Obj(Proc.Elem(gen))
    val liObj     = Obj(IntElem(li))
    genObj.attr.put("li", liObj)
    genObj.name = s"gen$li"
    lFolder.addLast(genObj)

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
    splitObj.name = s"split$li"
    lFolder.addLast(splitObj)
    pred.scans.add("out") ~> split.scans.add("in")

    // aka foreground splitter
    val succ        = Proc[S]
    succ.graph()    = splitGraph
    val succObj     = Obj(Proc.Elem(succ))
    succObj.name = s"succ$li"
    lFolder.addLast(succObj)
    gen.scans.add("out") ~> succ.scans.add("in")

    // aka collector
    val coll        = Proc[S]
    coll.graph()    = collGraph
    val collObj     = Obj(Proc.Elem(coll))
    collObj.name = s"coll$li"
    lFolder.addLast(collObj)

    // layer-ensemble output to successor
    val out         = Proc[S]
    out.graph()     = throughGraph
    out.scans.add("out")
    val outObj      = Obj(Proc.Elem(out))
    outObj.name = s"foo$li"
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
      // doneObj.attr.put("pred" , predObj )
      // doneObj.attr.put("out"  , outObj  )

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
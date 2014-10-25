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

import de.sciss.file._
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.{Expr, Boolean => BooleanEx, Int => IntEx}
import de.sciss.lucre.stm
import de.sciss.mellite.{Mellite, Workspace}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{SoundProcesses, BooleanElem, FolderElem, Folder, IntElem, Action, Code, Ensemble, ExprImplicits, Obj, Proc, Scan, graph}
import de.sciss.synth.{GE, SynthGraph}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.Future

object VoiceStructure {
  def main(args: Array[String]): Unit =
    add(userHome / "sysson" / "workspaces" / "layers_test.mllt")

  def add(workspace: File, path: Seq[String] = Nil): Unit = {
    val doc = Workspace.read(workspace)
    val fut = doc match {
      case cf : Workspace.Confluent =>
        implicit val cursor = cf.cursors.cursor
        cursor.step { implicit tx =>
          add(cf , path)
        }
      case dur: Workspace.Ephemeral =>
        implicit val cursor = dur.cursor
        cursor.step { implicit tx =>
          add(dur, path)
        }
    }
    import SoundProcesses.executionContext
    fut.foreach { _ =>
      println("Done.")
      sys.exit(0)
    }
  }

  def add[S <: Sys[S]](workspace: Workspace[S], path: Seq[String])
                      (implicit tx: S#Tx, cursor: stm.Cursor[S]): Future[Unit] = {
    import Motion.NavigateFolder

    println("Adding layers...")

    @tailrec def loop(parent: Folder[S], children: Seq[String]): Folder[S] =
      children match {
        case head +: tail =>
          val sub = parent / head match {
            case Some(FolderElem.Obj(f)) => f.elem.peer
            case Some(Ensemble  .Obj(e)) => e.elem.peer.folder
          }
          loop(sub, tail)
        case _ => parent
      }

    val child = loop(workspace.root(), path)
    import Mellite.compiler
    apply(parent = child)
  }

  def apply[S <: Sys[S]](parent: Folder[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                         compiler: Code.Compiler): Future[Unit] = {
    val vs        = new VoiceStructure[S]
    val actionFut = vs.mkAction(compiler)
    import Folder.serializer
    val parentH   = tx.newHandle(parent)
    import SoundProcesses.executionContext
    actionFut.map { actionH =>
      cursor.step { implicit tx =>
        val all     = vs.mkWorld(actionH())
        val parent  = parentH()
        // tx.newHandle(all)
        parent.addLast(all)
      }
    }
  }

  implicit class ScanOps[S <: Sys[S]](val `this`: Scan[S]) extends AnyVal {
    def ~> (that: Scan[S])(implicit tx: S#Tx): Unit =
      `this`.addSink(Scan.Link.Scan(that))

    def ~/> (that: Scan[S])(implicit tx: S#Tx): Unit =
      `this`.removeSink(Scan.Link.Scan(that))
  }
}
class VoiceStructure[S <: Sys[S]] {
  import VoiceStructure.ScanOps

  val DumpOSC         = false
  val ShowLog         = false
  var ShowNodeTree    = false   // warning - buggy
  val PrintStates     = false
  val Shadowing       = true
  val Attack          = 30  // 10
  val Release         = 30  // 10
  val FFTSize         = 512 // 1024

  val NumLayers       = 15  // 3
  val MaxVoices       = 3 // 2
  val NumSpeakers     = 42  // 2
  val NumTransitions  = 7   // 2

  private[this] val imp = ExprImplicits[S]
  import imp._

  //  class Layer(val ensemble:     stm.Source[S#Tx, Ensemble.Obj[S]],
  //              val states  : Vec[stm.Source[S#Tx, Expr.Var[S, Int    ]]],
  //              val playing :     stm.Source[S#Tx, Expr.Var[S, Boolean]],
  //              val transId :     stm.Source[S#Tx, Expr.Var[S, Int    ]],
  //              val input   :     stm.Source[S#Tx, Proc.Obj[S]],
  //              val output  :     stm.Source[S#Tx, Proc.Obj[S]])

  //  class World(val diffusion     :     stm.Source[S#Tx, Proc.Obj[S]],
  //              val layers        : Vec[Layer],
  //              val sensors       : Vec[stm.Source[S#Tx, Expr.Var[S, Int    ]]],
  //              val transId       :     stm.Source[S#Tx, Expr.Var[S, Int    ]],
  //              val activeVoices  :     stm.Source[S#Tx, Expr    [S, Int    ]],
  //              val hasFreeVoices :     stm.Source[S#Tx, Expr    [S, Boolean]])

  def rrand(lo: Int, hi: Int): Int = util.Random.nextInt(hi - lo + 1) + lo

  //  def mkAural(w: World)(implicit tx: S#Tx, cursor: stm.Cursor[S]): (AuralSystem, Transport[S]) = {
  //    val aural = AuralSystem()
  //    if (DumpOSC) aural.whenStarted(_.peer.dumpOSC())
  //    val transport = Transport[S](aural)
  //    transport.addObject(w.diffusion())
  //    w.layers.zipWithIndex.foreach { case (l, li) =>
  //      transport.addObject(l.ensemble())
  //    }
  //    transport.play()
  //    val config = Server.Config()
  //    config.audioBusChannels  = 4096 // 1024
  //    config.transport         = osc.TCP
  //    config.pickPort()
  //    aural.start(config)
  //    (aural, transport)
  //  }

//  def checkWorld(w: World, debug: Boolean = false)(implicit tx: S#Tx): Unit = {
//    val free  = w.hasFreeVoices()
//    val sense: Vec[Int] = w.sensors.map(_.apply().value)
//    w.layers.zipWithIndex /* .scramble() */.foreach { case (l, li) =>
//      if (PrintStates && debug) {
//        println(s"----- LAYER $li -----")
//      }
//
//      val isActive        = l.playing().value
//      val mayBecomeActive = !isActive && free.value
//
//      val hasFadeIn = (isActive || mayBecomeActive) && (false /: l.states.zipWithIndex) { case (res, (stateH, si)) =>
//        val state   = stateH()
//        val gate    = sense(si) == li
//        val before  = state().value
//        val now     = before match {
//          case 0 | 3 if  gate => 2
//          case 1 | 2 if !gate => 3
//          case _ => before
//        }
//        if (now != before) state() = now
//        res | (now == 2)
//      }
//
//      if (PrintStates && debug) {
//        println(l.states.zipWithIndex.map { case (s, si) => f"s$si%02d = ${s.apply().value}" } .mkString(", "))
//      }
//
//      val becomesActive = mayBecomeActive && hasFadeIn
//
//      if (becomesActive) {
//        if (PrintStates) println(s"Layer $li becomes active.")
//        l.transId().update(w.transId().value)
//        if (Shadowing) layerToFront(w, l)
//        l.playing().update(true)
//      }
//    }
//  }

//  def unlinkLayer(l: Layer)(implicit tx: S#Tx): Unit =
//    for {
//      layerIn  <- l.input    ().elem.peer.scans.get("in" )
//      layerOut <- l.output   ().elem.peer.scans.get("out")
//    } {
//      val oldLayerIn = layerIn.sources.collect {
//        case l @ Scan.Link.Scan(_) => l
//      } .toSet
//      val oldLayerOut = layerOut.sinks.collect {
//        case l @ Scan.Link.Scan(_) => l
//      } .toSet
//
//      // disconnect old inputs
//      oldLayerIn .foreach(layerIn .removeSource)
//      // disconnect old outputs
//      oldLayerOut.foreach(layerOut.removeSink  )
//      // connect old layer inputs to old layer outputs
//      oldLayerIn.foreach { in =>
//        oldLayerOut.foreach { out =>
//          in.peer.addSink(out)
//        }
//      }
//    }

//  def layerToFront(w: World, l: Layer)(implicit tx: S#Tx): Unit =
//    for {
//      layerIn  <- l.input    ().elem.peer.scans.get("in" )
//      layerOut <- l.output   ().elem.peer.scans.get("out")
//      diffIn   <- w.diffusion().elem.peer.scans.get("in" )
//    } {
//      val oldDiffIn = diffIn.sources.collect {
//        case l @ Scan.Link.Scan(_) => l
//      } .toSet
//      val layerOutL = Scan.Link.Scan(layerOut)
//      // only act if we're not there
//      if (!oldDiffIn.contains(layerOutL)) {
//        unlinkLayer(l)
//        // disconnect old diff inputs
//        oldDiffIn  .foreach(diffIn  .removeSource)
//        // connect old diff inputs as new layer inputs
//        oldDiffIn  .foreach(layerIn .addSource   )
//        // connect layer output to diff input
//        diffIn.addSource(layerOutL)
//      }
//    }

  def mkAction(c: Code.Compiler)(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                 compiler: Code.Compiler): Future[stm.Source[S#Tx, Action[S]]] = {
    val source =
      """val imp = ExprImplicits[S]
        |import imp._
        |
        |for {
        |  Expr.Var(state) <- self.attr[IntElem]("state")
        |} {
        |  state.transform(x => x.value match {
        |    case 2 => 1
        |    case 3 => 0
        |    case y => y // should not occur
        |  })
        |}
        |""".stripMargin
    implicit val compiler = c
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
      val atk    = 10.0
      val rls    = 10.0
      val in     = Select.kr(ToggleFF.kr(1), Seq(start, target))
      val fade   = Slew.kr(in, atk.reciprocal, rls.reciprocal)
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

  def mkWorld(done: Action[S])(implicit tx: S#Tx): Ensemble.Obj[S] = {
    val sensors = Vec.tabulate(NumSpeakers) { speaker =>
      val sensor = IntEx.newVar[S](-1)
      if (PrintStates) sensor.changed.react(_ => ch => println(s"sensor$speaker -> ${ch.now}"))
      sensor
    }

    val diff = Proc[S]
    diff.graph() = SynthGraph {
      import de.sciss.synth._
      import de.sciss.synth.ugen._
      val in = graph.ScanInFix(NumSpeakers)
      val mix = Mix.tabulate(NumSpeakers) { ch =>
        val inc = in \ ch
        val pan = if (NumSpeakers == 1) 0.0 else ch.linlin(0, NumSpeakers - 1, -1, 1)
        val sig = Pan2.ar(inc, pan)
        sig
      }
      Out.ar(0, mix)
    }
    diff.scans.add("in")
    val diffObj = Obj(Proc.Elem(diff))
    diffObj.attr.name = "diff"

    val vecLayer = Vec.tabulate(NumLayers) { li =>
      mkLayer(sensors, diff, done, li)
    }

    val vecPlaying      = vecLayer.map(_.elem.peer.playing)
    val activeVoices    = count(vecPlaying)
    val hasFreeVoices   = activeVoices < MaxVoices
    if (PrintStates) activeVoices.changed.react(_ => ch => println(s"activeVoices -> ${ch.now}"))

    val wTransId = IntEx.newVar[S](0)

    val allFolder = Folder[S]
    val all       = Ensemble(allFolder, 0L, BooleanEx.newVar[S](false))
    allFolder.addLast(diffObj)
    vecLayer.foreach(ensLObj => allFolder.addLast(ensLObj))
    val allObj    = Obj(Ensemble.Elem(all))
    sensors.zipWithIndex.foreach { case (s, si) =>
      allObj.attr.put(f"s$si%02d", Obj(IntElem(s)))
    }
    allObj.attr.put("trans", Obj(IntElem(wTransId)))
    allObj.attr.put("free-voices", Obj(BooleanElem(hasFreeVoices)))
    allObj.attr.name = "layers"
    allObj

    //    new World(diffusion     = tx.newHandle(diffObj),
    //      layers        = vecLayer,
    //      sensors       = sensors.map(tx.newHandle(_)),
    //      transId       = tx.newHandle(wTransId),
    //      activeVoices  = tx.newHandle(activeVoices),
    //      hasFreeVoices = tx.newHandle(hasFreeVoices))
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
    val dust  = Decay.ar(Dust.ar(Seq.fill(NumSpeakers)(10)), 1).min(1)
    val sig   = Resonz.ar(dust, freq, 0.5) * amp
    graph.ScanOut(sig)
  }

  // multi-channel single scan in, multiple signal-channel scan outs
  private lazy val splitGraph = SynthGraph {
    val in = graph.ScanInFix(NumSpeakers)
    Vec.tabulate(NumSpeakers) { ch =>
      graph.ScanOut(s"out$ch", in \ ch)
    }
  }

  // multiple signal-channel scan ins, multi-channel single scan out,
  private lazy val collGraph = SynthGraph {
    import de.sciss.synth.ugen._
    val in = Vec.tabulate(NumSpeakers) { ch =>
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

  private def mkLayer(sensors: Vec[Expr[S, Int]], diff: Proc[S], done: Action[S], li: Int)
                     (implicit tx: S#Tx): Ensemble.Obj[S] = {
    val transId = IntEx.newVar[S](-1) // "sampled" in `checkWorld`

    // layer-level ensemble
    val lFolder = Folder[S]

    // the actual sound layer
    val gen       = Proc[S]
    gen.graph()   = genGraph
    val genObj    = Obj(Proc.Elem(gen))
    val liObj     = Obj(IntElem(li))
    genObj.attr.put("li", liObj)
    genObj.attr.name = s"gen$li"
    lFolder.addLast(genObj)

    // layer-ensemble input from predecessor
    val pred        = Proc[S]
    pred.graph()    = throughGraph
    pred.scans.add("in")
    val predObj     = Obj(Proc.Elem(pred))
    predObj.attr.name = s"pred$li"
    lFolder.addLast(predObj)

    // aka background splitter
    val split       = Proc[S]
    split.graph()   = splitGraph
    val splitObj    = Obj(Proc.Elem(split))
    splitObj.attr.name = s"split$li"
    lFolder.addLast(splitObj)
    pred.scans.add("out") ~> split.scans.add("in")

    // aka foreground splitter
    val succ        = Proc[S]
    succ.graph()    = splitGraph
    val succObj     = Obj(Proc.Elem(succ))
    succObj.attr.name = s"succ$li"
    lFolder.addLast(succObj)
    gen.scans.add("out") ~> succ.scans.add("in")

    // aka collector
    val coll        = Proc[S]
    coll.graph()    = collGraph
    val collObj     = Obj(Proc.Elem(coll))
    collObj.attr.name = s"coll$li"
    lFolder.addLast(collObj)

    // layer-ensemble output to successor
    val out         = Proc[S]
    out.graph()     = throughGraph
    out.scans.add("out")
    val outObj      = Obj(Proc.Elem(out))
    outObj.attr.name = s"foo$li"
    lFolder.addLast(outObj)
    coll.scans.add("out") ~> out.scans.add("in")

    class Channel(val stateObj: Obj[S], val state: Expr.Var[S, Int], val fPlaying: Expr[S, Boolean],
                  val active: Expr[S, Boolean], val predOut: Scan[S], val succOut: Scan[S], val collIn: Scan[S],
                  val doneObj: Action.Obj[S])

    val vecChannels = Vec.tabulate[Channel](NumSpeakers) { si =>
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
      procBObj.attr.name = s"by$li$si"
      val bPlaying  = state <  2
      val bFolder   = Folder[S]
      bFolder.addLast(procBObj)
      val ensB      = Ensemble(bFolder, 0L, bPlaying)
      val ensBObj   = Obj(Ensemble.Elem(ensB))
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

      new Channel(stateObj = stateObj, state = state, fPlaying = fPlaying, active = active,
        predOut = predOut, succOut = succOut, collIn = collIn, doneObj = doneObj)
    }

    val activeCount = count(vecChannels.map(_.active))
    if (PrintStates) activeCount.changed.react(_ => ch => println(s"activeCount$li -> ${ch.now}"))

    val lPlaying    = BooleanEx.newVar[S](false)
    // if (PrintStates) lPlaying.changed.react(_ => ch => println(s"lPlaying$li -> ${ch.now}"))

    val ensL    = Ensemble[S](lFolder, 0L, lPlaying)
    val ensLObj = Obj(Ensemble.Elem(ensL))

    //    val bypassPlaying = !lPlaying
    //    val bypassF       = Folder[S]
    //    val ensBypass     = Ensemble[S](bypassF, 0L, bypassPlaying)
    //    val ensBypassObj  = Obj(Ensemble.Elem(ensBypass))
    //    val bypass        = Proc[S]
    //    bypass.graph()    = throughGraph
    //    val bypassObj     = Obj(Proc.Elem(bypass))
    //    bypassObj.attr.name = s"bypass$li"
    //    bypassF.addLast(bypassObj)
    //    pred  .scans.add("out") ~> bypass.scans.add("in")
    //    bypass.scans.add("out") ~> out   .scans.add("in")

    transGraphs.zipWithIndex.foreach { case (g, gi) =>
      val tPlaying    = transId sig_== gi
      val tFolder     = Folder[S]
      val ensT        = Ensemble[S](tFolder, 0L, tPlaying)
      val ensTObj     = Obj(Ensemble.Elem(ensT))
      lFolder.addLast(ensTObj)

      vecChannels.zipWithIndex.foreach { case (channel, si) =>
        val fFolder   = Folder[S]
        val ensF      = Ensemble(fFolder, 0L, channel.fPlaying)
        tFolder.addLast(Obj(Ensemble.Elem(ensF)))

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
        attr.name     = s"t$gi$si"
        attr.put("state", channel.stateObj)
        attr.put("done" , channel.doneObj )

        fFolder.addLast(procTObj)
      }
    }

    // short debug solution; just connect all layer outputs to main diffusion
    if (!Shadowing) coll.scans.add("out") ~> diff.scans.add("in")

    ensLObj
//    val states = vecChannels.map { channel => tx.newHandle(channel.state) }
//    val l = new Layer(
//      ensemble  = tx.newHandle(ensLObj),
//      states    = states,
//      playing   = tx.newHandle(lPlaying),
//      transId   = tx.newHandle(transId),
//      input     = tx.newHandle(predObj),
//      output    = tx.newHandle(outObj))
//    activeCount.changed.react { implicit tx => ch =>
//      if (ch.now == 0) {
//        if (PrintStates) println(s"Layer $li becomes inactive.")
//        l.playing().update(false)
//        unlinkLayer(l)
//      }
//    }
//    l
  }

  private def count(in: Vec[Expr[S, Boolean]])(implicit tx: S#Tx): Expr[S, Int] = {
    val imp = ExprImplicits[S]
    import imp._
    reduce(in.map(_.toInt))(_ + _)
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
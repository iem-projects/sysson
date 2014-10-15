package at.iem.sysson.turbulence

import de.sciss.desktop.DocumentHandler
import de.sciss.file._
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm
import de.sciss.lucre.swing.{defer, requireEDT}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.lucre.event.Sys
import de.sciss.mellite.{Mellite, Workspace, Application}
import de.sciss.mellite.gui.ActionOpenWorkspace
import de.sciss.synth.proc
import de.sciss.synth.proc.{Scan, Proc, Transport, ExprImplicits, AuralObj, IntElem, Elem, Ensemble, Obj, Folder, SoundProcesses}
import proc.Implicits._

import scala.concurrent.stm.{Ref, Txn}
import scala.language.higherKinds

object Motion {
  private var _instance: Option[Algorithm[_]] = None

  def instance: Option[Algorithm[_]] = {
    requireEDT()
    _instance
  }
  
  def run(start: Boolean): Unit = defer {
    _instance.foreach(_.stopGUI())
    _instance = None
    val f = Turbulence.baseDir / "workspaces" / "Turbulence.mllt"
    Application.documentHandler.addListener {
      case DocumentHandler.Added(doc: Workspace.Confluent) if doc.folder == f =>
        implicit val cursor = doc.cursors.cursor
        SoundProcesses.scheduledExecutorService.submit(new Runnable {
          def run(): Unit = {
            val i = cursor.step { implicit tx => init(doc, start = start) }
            defer { _instance = i }
          }
        })
    }
    ActionOpenWorkspace.perform(f)
  }

  private def warn(what: String): Unit = Console.err.println(s"WARNING: $what")

  private def info(what: String): Unit = Console.out.println(s"INFO: $what")

  implicit class NavigateFolder[S <: Sys[S]](private val folder: Folder[S]) extends AnyVal {
    def / (child: String)(implicit tx: S#Tx): Option[Obj[S]] = {
      val res = folder.iterator.filter { obj =>
        obj.attr.name == child
      } .toList.headOption

      if (res.isEmpty) warn(s"Child $child not found in $folder")
      res
    }
  }

  implicit class NavigateEnsemble[S <: Sys[S]](private val ensemble: Ensemble.Obj[S]) extends AnyVal {
    def / (child: String)(implicit tx: S#Tx): Option[Obj[S]] = {
      val res = ensemble.elem.peer.folder.iterator.filter { obj =>
        obj.attr.name == child
      }.toList.headOption

      if (res.isEmpty) warn(s"Child $child not found in ${ensemble.attr.name}")
      res
    }

    def play   ()(implicit tx: S#Tx): Unit = playGate(ensemble, value = true )
    def release()(implicit tx: S#Tx): Unit = playGate(ensemble, value = false)

    def isPlaying(implicit tx: S#Tx): Boolean = ensemble.elem.peer.playing.value
  }

  implicit class ResolveObj[S <: Sys[S]](private val obj: Obj[S]) {
    def as[E[~ <: Sys[~]] <: Elem[~]](implicit tx: S#Tx, companion: Elem.Companion[E]): Option[E[S]#Peer] = {
      val res = if (obj.elem.typeID == companion.typeID) Some(obj.elem.peer.asInstanceOf[E[S]#Peer]) else None
      if (res.isEmpty) warn(s"Object $obj is not of type ${companion.typeID.toHexString}")
      res
    }
  }

  private def init[S <: SSys[S]](workspace: Workspace[S], start: Boolean)
                                (implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[Algorithm[S]] = {
    val root = workspace.root()
    for {
      Ensemble.Obj(layers) <- root / "layers"
    } yield {
      // println(s"State is ${state.value}")
      info("Play layers")
      val t = Transport[S](Mellite.auralSystem)
      t.addObject(layers)
      // AuralObj(layers).play()
      // t.play()

      val alg = new Algorithm(tx.newHandle(layers), t)
      if (start) alg.start()
      alg
    }
  }

  def rrand(lo: Double, hi: Double) = math.random * (hi - lo) + lo

  def getScan[S <: Sys[S]](p: Proc.Obj[S], key: String)(implicit tx: S#Tx): Option[Scan[S]] = {
    val res = p.elem.peer.scans.get(key)
    if (res.isEmpty) warn(s"Scan $key not found in ${p.attr.name}")
    res
  }

  def setInt[S <: Sys[S]](obj: Obj[S], key: String, value: Int, quiet: Boolean = false)(implicit tx: S#Tx): Unit = {
    val res = obj.attr[IntElem](key).collect {
      case Expr.Var(vr) =>
        val imp = ExprImplicits[S]
        import imp._
        vr() = value
    }
    if (res.isEmpty && !quiet) warn(s"Int attr $key not found / a var, in ${obj.attr.name}")
  }

  def toggleFilter[S <: Sys[S]](p: Proc.Obj[S], pred: Proc.Obj[S], succ: Proc.Obj[S], bypass: Boolean)(implicit tx: S#Tx): Unit =
    for {
      predOut   <- getScan(pred, "out")
      succIn    <- getScan(succ, "in" )
      filterIn  <- getScan(p,    "in" )
      filterOut <- getScan(p,    "out")
    } {
      if (bypass) {
        predOut  .addSink     (Scan.Link.Scan(succIn   ))
        predOut  .removeSink  (Scan.Link.Scan(filterIn ))
        filterOut.removeSink  (Scan.Link.Scan(succIn   ))
      } else {
        filterOut.addSink     (Scan.Link.Scan(succIn   ))
        predOut  .addSink     (Scan.Link.Scan(filterIn ))
        predOut  .removeSink  (Scan.Link.Scan(succIn   ))
      }
    }

  def play[S <: Sys[S]](obj: Ensemble.Obj[S], value: Boolean)(implicit tx: S#Tx): Unit = {
    val res = Expr.Var.unapply(obj.elem.peer.playing)
    res.foreach { vr =>
      val imp = ExprImplicits[S]
      import imp._
      vr() = value
    }
    if (res.isEmpty) println(s"WARNING: Ensemble playing not a var")
  }

  def playGate[S <: Sys[S]](obj: Ensemble.Obj[S], value: Boolean)(implicit tx: S#Tx): Unit = {
    setInt(obj, "gate", if (value) 1 else 0, quiet = value)
    if (value) play(obj, value)
  }

  class Algorithm[S <: SSys[S]](layersH: stm.Source[S#Tx, Ensemble.Obj[S]], transport: Transport[S])
                              (implicit cursor: stm.Cursor[S]) {
    private val imp = ExprImplicits[S]
    import imp._
    
    def start()(implicit tx: S#Tx): Unit = {
      stopAll()
      iterate()
      transport.play()
    }
    
    def startGUI(): Unit = cursor.step { implicit tx => start() }
    
    def stop()(implicit tx: S#Tx): Unit = transport.stop()

    def stopGUI(): Unit = cursor.step { implicit tx => stop() }

    def layers(implicit tx: S#Tx) = layersH()

    def after(secs: Double)(code: S#Tx => Unit): Unit = {
      val t = new Thread {
        override def run(): Unit = {
          Thread.sleep((secs * 1000).toLong)
          cursor.step { implicit tx =>
            if (transport.isPlaying) code(tx)
          }
        }
      }
      Txn.findCurrent.fold(t.start()) { implicit tx =>
        Txn.afterCommit(_ => t.start())
      }
    }

    ////////////////////////////////////////////

    def stopAll()(implicit tx: S#Tx): Unit = toggleData1(value = false)

    def toggleData1(value: Boolean)(implicit tx: S#Tx): Unit =
      for {
        Proc.Obj(col1)     <- layers  / "col-1"
        Proc.Obj(col2)     <- layers  / "out"
        Ensemble.Obj(bgF)  <- layers  / "bg-filter"
        Proc.Obj(bgFP)     <- bgF     / "proc"
        Ensemble.Obj(data) <- layers  / "data-1"
      } {
        info(s"---- ${if (value) "play" else "stop"} data-1 ----")
        play(bgF, value = value)
        toggleFilter(bgFP, pred = col1, succ = col2, bypass = !value)

        playGate(data, value = value)
      }

    def iterate()(implicit tx: S#Tx): Unit =
      if (layers.isPlaying) playFreesound()

    def playFreesound()(implicit tx: S#Tx): Unit =
      for {
        Ensemble.Obj(freesound) <- layers / "freesound"
      } {
        info("---- start freesound ----")
        freesound.play()
        val d = 20 // rrand(30, 60)
        after(d) { implicit tx => enterData1() }
      }

    def enterData1()(implicit tx: S#Tx): Unit = {
      toggleData1(value = true)
      val d = 20 // rrand(30, 60)
      after(d) { implicit tx => stopFreesound() }
    }
    
    def stopFreesound()(implicit tx: S#Tx): Unit =
      for {
        Ensemble.Obj(freesound) <- layers / "freesound"
      } {
        info("---- stop freesound ----")
        freesound.release()
        val d = 20 // rrand(30, 60)
        after(d) { implicit tx =>
          stopAll()
          iterate()
        }
      }
  }

  // E freesound
  // E layer-1
  // P col-1
  // E bg-filter
  // E data-1
  // P out
}

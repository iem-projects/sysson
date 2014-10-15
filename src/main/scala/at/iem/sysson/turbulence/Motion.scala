package at.iem.sysson.turbulence

import de.sciss.desktop.DocumentHandler
import de.sciss.file._
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm
import de.sciss.lucre.swing.defer
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.lucre.event.Sys
import de.sciss.mellite.{Mellite, Workspace, Application}
import de.sciss.mellite.gui.ActionOpenWorkspace
import de.sciss.synth.proc
import de.sciss.synth.proc.{Transport, ExprImplicits, AuralObj, IntElem, Elem, Ensemble, Obj, Folder, SoundProcesses}
import proc.Implicits._

import scala.language.higherKinds

object Motion extends Runnable {
  def run(): Unit = defer {
    val f = Turbulence.baseDir / "workspaces" / "Turbulence.mllt"
    Application.documentHandler.addListener {
      case DocumentHandler.Added(doc: Workspace.Confluent) if doc.folder == f =>
        implicit val cursor = doc.cursors.cursor
        SoundProcesses.scheduledExecutorService.submit(new Runnable {
          def run(): Unit = cursor.step { implicit tx => init(doc) }
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
    def / (child: String)(implicit tx: S#Tx): Option[Obj[S]] = ensemble.elem.peer.folder / child

    def play()(implicit tx: S#Tx): Unit = ensemble.elem.peer.playing match {
      case Expr.Var(vr) =>
        val imp = ExprImplicits[S]
        import imp._
        vr() = true
      case _ => warn(s"$ensemble - playing not a variable")
    }
  }

  implicit class ResolveObj[S <: Sys[S]](private val obj: Obj[S]) {
    def as[E[~ <: Sys[~]] <: Elem[~]](implicit tx: S#Tx, companion: Elem.Companion[E]): Option[E[S]#Peer] = {
      val res = if (obj.elem.typeID == companion.typeID) Some(obj.elem.peer.asInstanceOf[E[S]#Peer]) else None
      if (res.isEmpty) warn(s"Object $obj is not of type ${companion.typeID.toHexString}")
      res
    }
  }

  private def init[S <: SSys[S]](workspace: Workspace[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
    val root = workspace.root()
    for {
      Ensemble.Obj(layers)    <- root / "layers"
      Ensemble.Obj(freesound) <- layers / "freesound"
    } {
      // println(s"State is ${state.value}")
      info("Play layers")
      val t = Transport[S](Mellite.auralSystem)
      t.addObject(layers)
      // AuralObj(layers).play()
      t.play()

      freesound.play()
    }
  }

  // E freesound
  // E layer-1
  // P col-1
  // E bg-filter
  // E data-1
  // P out

  /*
  // val a = self.attr

// ---- utility functions ----

val imp = ExprImplicits[S]
import imp._  // XXX TODO - this should be provided

def getInt(obj: Obj[S], key: String): Option[Int] = {
  val res = obj.attr[IntElem](key).map(_.value)
  if (res.isEmpty) println(s"WARNING: Int attr $key not found")
  res
}

def getProc(obj: Obj[S], key: String): Option[Proc[S]] = {
  val res = obj.attr[Proc.Elem](key)
  if (res.isEmpty) println(s"WARNING: Proc attr $key not found")
  res
}

def getEnsemble(obj: Obj[S], key: String): Option[Ensemble.Obj[S]] = {
  val res = obj.attr.get(key).collect {
    case Ensemble.Obj(e) => e
  }
  if (res.isEmpty) println(s"WARNING: Ensemble attr $key not found")
  res
}

def getObj(obj: Obj[S], key: String): Option[Obj[S]] = {
  val res = obj.attr.get(key)
  if (res.isEmpty) println(s"WARNING: Attr $key not found")
  res
}

def setInt(obj: Obj[S], key: String, value: Int): Unit = {
  val res = obj.attr[IntElem](key).collect {
    case Expr.Var(vr) => vr() = value
  }
  if (res.isEmpty) println(s"WARNING: Int attr $key not found / a var")
}

def setBoolean(obj: Obj[S], key: String, value: Boolean): Unit = {
  val res = obj.attr[BooleanElem](key).collect {
    case Expr.Var(vr) => vr() = value
  }
  if (res.isEmpty) println(s"WARNING: Boolean attr $key not found / a var")
}

def play(obj: Ensemble.Obj[S], value: Boolean): Unit = {
  val res = Expr.Var.unapply(obj.elem.peer.playing)
  res.foreach(vr => vr() = value)
  if (res.isEmpty) println(s"WARNING: Ensemble playing not a var")
}

def playGate(obj: Ensemble.Obj[S], value: Boolean): Unit = {
  setInt(obj, "gate", if (value) 1 else 0)
  if (value) play(obj, value)
}

def getScan(p: Proc[S], key: String): Option[Scan[S]] = {
  val res = p.scans.get(key)
  if (res.isEmpty) println(s"WARNING: Scan $key not found")
  res
}

def toggleFilter(p: Proc[S], pred: Proc[S], succ: Proc[S], bypass: Boolean): Unit =
  for {
    predOut   <- getScan(pred, "out")
    succIn    <- getScan(succ, "in" )
    filterIn  <- getScan(p,    "in" )
    filterOut <- getScan(p,    "out")
  } {
    predOut  .removeSink  (Scan.Link.Scan(filterIn ))
    filterOut.removeSink  (Scan.Link.Scan(succIn   ))
    predOut  .removeSink  (Scan.Link.Scan(succIn   ))

    if (bypass) {
      predOut  .addSink     (Scan.Link.Scan(succIn   ))
    } else {
      filterOut.addSink     (Scan.Link.Scan(succIn   ))
      predOut  .addSink     (Scan.Link.Scan(filterIn ))
    }
  }

// ---- individual reactions ----

def returnToBg(): Boolean = {
  println("---- return to background ----")
  val res = for {
    bg   <- getEnsemble(self, "bg")
    data <- getEnsemble(self, "data-1")
  } yield {
    playGate(bg  , true )
    playGate(data, false)
  }
  res.isDefined
}

def firstData(): Boolean = {
  println("---- engage first data ----")
  val res = for {
    col1 <- getProc    (self, "col-1")
    col2 <- getProc    (self, "col-2")
    bgF  <- getEnsemble(self, "bg-filter")
    bgFP <- getProc    (bgF , "proc")
    data <- getEnsemble(self, "data-1")
  } yield {
    // playGate(bgF , true)
    play(bgF, true)
    toggleFilter(bgFP, pred = col1, succ = col2, bypass = false)
    playGate(data, true)
  }
  res.isDefined
}

def secondData(): Boolean = {
  println("---- engage second data ----")
  false
}

// ---- main ----

for {
  // 0 - background (present), 1 - data1, 2 - data2
  state <- getInt(self, "state")
} {
  val ok = state match {
    case 0 => returnToBg()
    case 1 => firstData()
    // case 2 => secondData()
    case _ =>
      println(s"Unexpected state value: $state")
      false
  }
  if (ok) {
    setInt(self, "state", (state + 1) % 2 /* 3 */)
  } else {
    sys.error("not yet working")
  }
}

   */
}

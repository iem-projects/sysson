/*
 *  MakeWorkspace.scala
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
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.mellite
import de.sciss.mellite.{Mellite, Workspace}
import de.sciss.synth.proc
import de.sciss.synth.proc.{Ensemble, FolderElem, Folder, SoundProcesses}
import proc.Implicits._

import scala.annotation.tailrec
import scala.concurrent.Future

object MakeWorkspace {
  val DEBUG = false

  def main(args: Array[String]): Unit = {
    // de.sciss.lucre.stm.showLog = true
    mellite.initTypes()

    val dir = Turbulence.baseDir / "workspaces"
    dir.mkdirs()
    val f   = dir / "turbulence.mllt"
    if (!f.exists()) {
      // val ws = Workspace.Confluent.empty(f, BerkeleyDB.Config())
      val ws = Workspace.Durable.empty(f, BerkeleyDB.Config())
      ws.close()
    }
    run(f)
  }

  private def getParent[S <: Sys[S]](workspace: Workspace[S], path: Seq[String])
                                    (implicit tx: S#Tx): Folder[S] = {
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

    val child = loop(workspace.root, path)
    child
  }

  private def run(workspace: File): Unit = {
    val doc = Workspace.read(workspace, BerkeleyDB.Config())
    val fut = doc match {
      case doc1: Workspace.Confluent =>
        implicit val ws = doc1
        build()
      case doc1: Workspace.Durable =>
        implicit val ws = doc1
        build()
      case doc1: Workspace.InMemory =>
        implicit val ws = doc1
        build()
    }
    import SoundProcesses.executionContext
    fut.foreach { _ =>
      println("Done.")
      sys.exit(0)
    }
  }

  private def build[S <: Sys[S]]()(implicit workspace: Workspace[S]): Future[Unit] = {
    implicit val compiler = Mellite.compiler
    implicit val cursor: stm.Cursor[S] = workspace.cursor
    val fut1 = cursor.step { implicit tx =>
      VoiceStructure()
    }
    import SoundProcesses.executionContext
    fut1.flatMap { _ =>
      cursor.step { implicit tx =>
        MakingWaves(getParent(workspace, Nil))
      }
    }
  }
}

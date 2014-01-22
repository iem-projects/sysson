/*
 *  MenuFactory.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import java.awt.event.KeyEvent
import de.sciss.desktop.{KeyStrokes, Menu}
import scala.swing.Action
import de.sciss.lucre.event.{Sys, Durable}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.file._
import de.sciss.lucre.stm
import language.existentials

object MenuFactory {
  def root: Menu.Root = _root

  private lazy val _root = {
    import Menu._
    import KeyEvent._
    import KeyStrokes._

    val dh = DocumentHandler.instance

    val actionCloseAll = new Action("Close All") {
      accelerator = Some(menu1 + shift + VK_W)
      def apply(): Unit = closeAll()
    }

    def checkCloseAll(): Unit = actionCloseAll.enabled = !dh.isEmpty

    checkCloseAll()

    dh.addListener {
      case DocumentHandler.Opened(doc) =>
        // recent.add(doc.dir)
        checkCloseAll()

      case DocumentHandler.Closed(doc) =>
        checkCloseAll()
    }

    Root().add(
      Group("file", "File").add(
        Group("new", "New").add(
          Item("new-workspace", ActionNewWorkspace) // ("Workspace..." -> (menu1 + VK_N)) {
        ).addLine()
        .add(
          Item("new-interpreter")("Interpreter..." -> (menu1 + VK_R)) {
            openInterpreter()
          }
        ).add(
          Item("new-library")("Library..." -> (menu1 + VK_L)) {
            openLibrary()
          }
        )
      ).add(Item("open", ActionOpenWorkspace))
      .add(ActionOpenWorkspace.recentMenu)
      .addLine().add(
        Item("close", proxy("Close" -> (menu1 + VK_W)))
      ).add(
        Item("close-all", actionCloseAll)
      ).add(
        Item("save", proxy("Save" -> (menu1 + VK_S)))
      )
    ).add(
      Group("edit", "Edit").add(
        Item("undo", proxy("Undo" -> (menu1 + VK_Z)))
      ).add(
        Item("redo", proxy("Redo" -> (menu1 + shift + VK_Z)))
      )
    ).add(
      Group("tools", "Tools")
        // .add(
        //   Item("sonif-declaration")("Sonification Declaration TEST..." -> (menu1 + VK_D)) { sonif.DeclarationEditor() }
        // )
      //      .add(
      //        Item("designer")("Sound Designer..." -> (menu1 + VK_D)) { openSoundDesigner() }
      //      )
    ).add(
      Group("view", "View").add(
        Item("clear-log")("Clear Log Window" -> (menu1 + shift + VK_P)) {
          LogWindow.instance.log.clear()
        }
      )
    ).add(
      Group("window", "Window")
    )
  }

  def closeAll(): Unit = {
    val docs = DocumentHandler.instance.allDocuments

    // cf. http://stackoverflow.com/questions/20982681/existential-type-or-type-parameter-bound-failure
    def screwYou[S <: Sys[S]](doc: Workspace[S]): Unit = doc.cursor.step { implicit tx => doc.dispose() }

    docs.foreach(doc => screwYou(doc.asInstanceOf[Workspace[~] forSome { type ~ <: Sys[~] }]))
  }

  //  def openSoundDesigner(): Unit =
  //    sound.designer.DesignerView()

  def openInterpreter(): Unit = InterpreterView()

  private type S = Durable
  private implicit lazy val system: S = {
    val store = BerkeleyDB.factory(dir = syssonDir / "library")
    Durable(store)
  }

  private lazy val libraryH: stm.Source[S#Tx, Library[S]] =
    system.root { implicit tx =>
      val _lib  = Library[S]
      //      val imp   = ExprImplicits[S]
      //      import imp._
      //      _lib.root.insertLeaf  (0, "Test-Leaf", "Test-Source")
      //      val sub = _lib.root.insertBranch(0, "Test-Branch")
      //      sub.insertLeaf       (0, "Test-Child", "Test-Source")
      _lib
    }

  def openLibrary(): Unit =
    system.step { implicit tx =>
      val lib = libraryH()
      LibraryWindow(lib)
    }
}
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

import de.sciss.desktop.{OptionPane, Desktop, KeyStrokes, Menu}
import scala.swing.{Label, Action}
import de.sciss.lucre.event.{Sys, Durable}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.file._
import de.sciss.lucre.stm
import gui.{SwingApplication => App}
import language.existentials
import at.iem.sysson.sound.AudioSystem
import de.sciss.{osc, synth}
import scala.swing.event.{Key, MouseClicked}
import java.net.URL

object MenuFactory {
  def root: Menu.Root = _root

  private lazy val _root = {
    import Menu._
    import KeyStrokes._

    val dh = DocumentHandler.instance

    val actionCloseAll = new Action("Close All") {
      accelerator = Some(menu1 + shift + Key.W)
      def apply(): Unit = closeAll()
    }

    def checkCloseAll(): Unit = actionCloseAll.enabled = !dh.isEmpty

    checkCloseAll()

    dh.addListener {
      case DocumentHandler.Opened(doc) =>
        // recent.add(doc.dir)Main
        checkCloseAll()

      case DocumentHandler.Closed(doc) =>
        checkCloseAll()
    }

    val itAbout = Item.About(App) {
      val addr    = "sysson.kug.ac.at"
      val url     = s"http://$addr/"
      val version = Main.version
      val html =
        s"""<html><center>
           |<font size=+1><b>About ${App.name}</b></font><p>
           |Version $version<p>
           |<p>
           |Copyright (c) 2013&ndash;2014 Institute of Electronic Music and Acoustics, Graz.<p>
           |Written by Hanns Holger Rutz.<p>
           |This software is published under the GNU General Public License v2+<p>
           |<p>
           |SysSon is a research project of IEM / Kunst Uni Graz<p>
           |in collaboration with Wegener Center for Climate and Global Change<p>
           |and funded by the Austrian Science Fund (FWF).<p>
           |<p>
           |<a href="$url">$addr</a>
           |""".stripMargin
      val lb = new Label(html) {
        // cf. http://stackoverflow.com/questions/527719/how-to-add-hyperlink-in-jlabel
        // There is no way to directly register a HyperlinkListener, despite hyper links
        // being rendered... A simple solution is to accept any mouse click on the label
        // to open the corresponding website.
        cursor = java.awt.Cursor.getPredefinedCursor(java.awt.Cursor.HAND_CURSOR)
        listenTo(mouse.clicks)
        reactions += {
          case MouseClicked(_, _, _, 1, false) => Desktop.browseURI(new URL(url).toURI)
        }
      }

      OptionPane.message(message = lb.peer).show(None /* Some(frame) */)
    }

    val itPrefs = Item.Preferences(App)(ActionPreferences())
    val itQuit  = Item.Quit(App)

    val gFile = Group("file", "File")
      .add(Group("new", "New")
        .add(
          Item("new-workspace", ActionNewWorkspace) // ("Workspace..." -> (menu1 + VK_N)) {
        )
        .addLine()
        .add(Item("new-interpreter")("Interpreter..." -> (menu1 + Key.R)) {
            openInterpreter()
          }
        )
        .add(
          Item("new-library")("Library..." -> (menu1 + Key.L)) {
            openLibrary()
          }
        )
      )
      .add(Item("open", ActionOpenWorkspace))
      .add(ActionOpenWorkspace.recentMenu)
      .addLine()
      .add(Item("close", proxy("Close" -> (menu1 + Key.W))))
      .add(Item("close-all", actionCloseAll))
      .add(Item("save", proxy("Save" -> (menu1 + Key.S))))

    if (itQuit.visible) gFile.addLine().add(itQuit)

    val gEdit = Group("edit", "Edit")
    val keyRedo = if (Desktop.isWindows) menu1 + Key.Y else menu1 + shift + Key.Z
    gEdit
      .add(Item("undo", proxy("Undo" -> (menu1 + Key.Z))))
      .add(Item("redo", proxy("Redo" -> keyRedo)))
    if (itPrefs.visible && Desktop.isLinux) gEdit.addLine().add(itPrefs)

    val gTools = Group("tools", "Tools")
    val gDebug = Group("debug", "Debug")
    gDebug
      .add(Item("dump-osc")("Dump OSC" -> (ctrl + shift + Key.D))(dumpOSC()))

    gTools.add(gDebug)
    if (itPrefs.visible && !Desktop.isLinux) gTools.addLine().add(itPrefs)

    val gView = Group("view", "View")
      .add(
        Item("clear-log")("Clear Log Window" -> (menu1 + shift + Key.P)) {
          LogWindow.instance.log.clear()
        }
      )
    val gWindow = Group("window", "Window")
    //  .add(Item("windowShot",         proxy("Export Window as PDF...")))

    val r = Root().add(gFile).add(gEdit).add(gTools).add(gView).add(gWindow)
    if (itAbout.visible) r.add(Group("help", "Help").add(itAbout))
    r
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

  private var resp = Option.empty[synth.message.Responder]

  private var dumpMode: osc.Dump = osc.Dump.Off

  def dumpOSC(): Unit = AudioSystem.instance.server.foreach { s =>
    dumpMode = if (dumpMode == osc.Dump.Off) osc.Dump.Text else osc.Dump.Off
    s.peer.dumpOSC(dumpMode)
  }

  private def dumpOSC_OLD(): Unit = {
    resp.fold {
      resp = AudioSystem.instance.server.map { s =>
        logInfo("Dump OSC on")
        synth.message.Responder.add(s.peer) {
          case m => if (m.name != "/$meter" && m.name != "/status.reply")
            System.out.synchronized {
              osc.Packet.printTextOn(m, osc.PacketCodec.default, System.out)
            }
        }
      }
    } { r =>
      logInfo("Dump OSC off")
      r.remove()
      resp = None
    }
  }
}
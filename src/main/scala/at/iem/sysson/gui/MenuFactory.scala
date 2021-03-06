/*
 *  MenuFactory.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import java.net.{URI, URL}

import at.iem.sysson.gui.impl.ActionConvertSpreadsheet
import at.iem.sysson.gui.{SwingApplication => App}
import de.sciss.desktop.{Desktop, KeyStrokes, Menu, OptionPane}
import de.sciss.lucre.synth.Txn
import de.sciss.mellite.Mellite
import de.sciss.mellite.gui.{ActionBounce, ActionCloseAllWorkspaces, ActionNewWorkspace, ActionOpenWorkspace, ActionPreferences, LogFrame}
import de.sciss.osc
import de.sciss.synth.{Client, Server}

import scala.concurrent.stm.TxnExecutor
import scala.swing.Label
import scala.swing.event.{Key, MouseClicked}
import scala.util.control.NonFatal

// XXX TODO --- remind me again, why do we need to copy Mellite's one?
object MenuFactory {
  def root: Menu.Root = _root

  private lazy val _root = {
    import KeyStrokes._
    import de.sciss.desktop.Menu._

    def funAbout(): Unit = {
      val addr    = "sysson.iem.at"
      val url     = s"http://$addr/"
      val name    = Main.name
      val version = Main.version
      val jreInfo: String = {
        val name    = sys.props.getOrElse("java.runtime.name"   , "?")
        val version = sys.props.getOrElse("java.runtime.version", "?")
        s"$name (build $version)"
      }
      val scalaVersion = try {
        val clazz = Class.forName("at.iem.sysson.BuildInfo")
        val m     = clazz.getMethod("scalaVersion")
        m.invoke(null).toString
      } catch {
        case NonFatal(_) => "?"
      }
      val sConfig = Server.Config()
      val cConfig = Client.Config()
      Mellite.applyAudioPreferences(sConfig, cConfig, useDevice = true, pickPort = true)
      val scVersion = Server.version(sConfig).toOption.fold {
        "Unknown SuperCollider version"
      } { case (v, b) =>
        val bs = if (b.isEmpty) b else s" ($b)"
        s"SuperCollider v$v$bs"
      }

      val html =
        s"""<html><center>
           |<font size=+1><b>About $name</b></font><p>
           |Version $version<p>
           |<p>
           |Copyright (c) 2013&ndash;2017 Institute of Electronic Music and Acoustics, Graz.<p>
           |Written by Hanns Holger Rutz.<p>
           |This software is published under the GNU General Public License v3+<p>
           |<p>
           |SysSon is a research project of IEM / Kunst Uni Graz<p>
           |in collaboration with Wegener Center for Climate and Global Change.<p>
           |<p>
           |SySon was created with funding by the Austrian Science Fund FWF (P 24159).<br>
           |Further development funded by the Austrian Knowledge Transfer Centre (WTZ S&uuml;d).
           |<p>
           |<a href="$url">$addr</a>
           |<p><br><hr>
           |<i>Scala v$scalaVersion<i><br>
           |<i>$jreInfo<br>
           |$scVersion
           |</i>""".stripMargin
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
    val itAbout = try {
      Item.About(App)(funAbout())
    } catch {
      case _: Throwable => Item("about")("About")(funAbout())  // yes, not cool
    }

    val itPrefs = try {
      Item.Preferences(App)(ActionPreferences())
    } catch {
      case _: Throwable => Item("preferences")("Preferences")(ActionPreferences()) // yes, not cool
    }
    val itQuit = try {
      Item.Quit(App)
    } catch {
      case _: Throwable => Item("quit")("Quit")(App.quit())
    }

    val gFile = Group("file", "File")
      .add(Group("new", "New")
        .add(Item("new-basic-workspace")("Workspace..." -> (menu1 + shift + Key.N)) {
          ActionNewWorkspace.performDurable()
        })
        .add(Item("new-workspace")("Extended Workspace...")(ActionNewWorkspace()))
        .addLine()
        .add(Item("new-interpreter")("Interpreter..." -> (menu1 + Key.R)) {
            openInterpreter()
          }
        )
      )
      .add(Group("convert", "Convert")
        .add(
          Item("convert-spreadsheet", ActionConvertSpreadsheet)
        )
      )
      .add(Item("open", ActionOpenWorkspace))
      .add(ActionOpenWorkspace.recentMenu)
      .addLine()
      .add(Item("close" , proxy("Close" -> (menu1 + Key.W))))
      .add(Item("close-all", ActionCloseAllWorkspaces))
      .add(Item("save"  , proxy("Save" -> (menu1 + Key.S))))
      .add(Item("bounce", proxy((s"${ActionBounce.title}...", menu1 + Key.B))))

    if (itQuit.visible) gFile.addLine().add(itQuit)

    val gEdit = Group("edit", "Edit")
    val keyRedo = if (Desktop.isWindows) menu1 + Key.Y else menu1 + shift + Key.Z
    gEdit
      .add(Item("undo", proxy("Undo" -> (menu1 + Key.Z))))
      .add(Item("redo", proxy("Redo" -> keyRedo)))
      .addLine()
      .add(Item("cut",                proxy(("Cut",                     menu1 + Key.X))))
      .add(Item("copy",               proxy(("Copy",                    menu1 + Key.C))))
      .add(Item("paste",              proxy(("Paste",                   menu1 + Key.V))))
      .add(Item("delete",             proxy(("Delete",                  plain + Key.BackSpace))))
      .addLine()
      .add(Item("select-all",         proxy(("Select All",              menu1 + Key.A))))

    if (itPrefs.visible /* && Desktop.isLinux */) gEdit.addLine().add(itPrefs)

    val gActions = Group("actions", "Actions")
    val gDebug = Group("debug", "Debug")
    gDebug
      .add(Item("dump-osc")("Dump OSC" -> (ctrl + shift + Key.D))(dumpOSC()))
      // .add(Item("debug-print", proxy("Debug Print", menu2 + Key.P)))
      .add(Item("toggle-log")("Debug Logging")(toggleLog()))

    gActions
      .add(Item("stop-all-sound",     proxy(("Stop All Sound",          menu1 + Key.Period))))
      .add(Item("debug-print",        proxy(("Debug Print",             menu2 + Key.P))))
      .add(Item("window-shot",        proxy("Export Window as PDF...")))

    // if (itPrefs.visible && !Desktop.isLinux) gTools.addLine().add(itPrefs)

    val gView = Group("view", "View")
      .add(Item("show-log" )("Show Log Window"  -> (menu1         + Key.P))(logToFront()))
      .add(Item("clear-log")("Clear Log Window" -> (menu1 + shift + Key.P))(clearLog  ()))
    val gWindow = Group("window", "Window")

    val gHelp = Group("help", "Help")
    if (itAbout.visible) gHelp.add(itAbout)
    gHelp
      .add(Item("index")("Online Documentation")(
        Desktop.browseURI(new URI("https://sysson.iem.at/documentation.html"))))
      .add(Item("issues")("Report a Bug")(
        Desktop.browseURI(new URI("https://github.com/iem-projects/sysson/issues"))))

    val r = Root().add(gFile).add(gEdit).add(gActions).add(gView).add(gWindow).add(gDebug).add(gHelp)

    r
  }

  def closeAll(): Unit = ActionCloseAllWorkspaces()

  def openInterpreter(): Unit = InterpreterView()

//  private type S = Durable

//  private implicit lazy val system: S = {
//    val store = BerkeleyDB.factory(dir = syssonDir / "library")
//    Durable(store)
//  }

  private var dumpMode: osc.Dump = osc.Dump.Off

  def dumpOSC(): Unit = {
    val sOpt = TxnExecutor.defaultAtomic { itx =>
      implicit val tx: Txn = Txn.wrap(itx)
      Mellite.auralSystem.serverOption
    }
    sOpt.foreach { s =>
      dumpMode = if (dumpMode == osc.Dump.Off) osc.Dump.Text else osc.Dump.Off
      s.peer.dumpOSC(dumpMode, filter = {
        case m: osc.Message if m.name == "/$meter" => false
        case _ => true
      })
      println(s"DumpOSC is ${if (dumpMode == osc.Dump.Text) "ON" else "OFF"}")
    }
  }

  def clearLog  (): Unit = LogFrame.instance.log.clear()
  def logToFront(): Unit = LogFrame.instance.front()  // XXX TODO - should avoid focus transfer

  def toggleLog(): Unit = {
    val enabled = !showLog
    showLog                               = enabled
    de.sciss.synth.proc.showLog           = enabled
    de.sciss.synth.proc.showAuralLog      = enabled
    de.sciss.synth.proc.showTransportLog  = enabled
    println(s"Logging is ${if (enabled) "ON" else "OFF"}")
  }

  //  private def dumpOSC_OLD(): Unit = {
  //    resp.fold {
  //      resp = AudioSystem.instance.server.map { s =>
  //        logInfo("Dump OSC on")
  //        synth.message.Responder.add(s.peer) {
  //          case m => if (m.name != "/$meter" && m.name != "/status.reply")
  //            System.out.synchronized {
  //              osc.Packet.printTextOn(m, osc.PacketCodec.default, System.out)
  //            }
  //        }
  //      }
  //    } { r =>
  //      logInfo("Dump OSC off")
  //      r.remove()
  //      resp = None
  //    }
  //  }
}
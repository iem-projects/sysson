/*
 *  SwingApplication.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import java.awt.Color
import java.util.Locale
import javax.swing.UIManager
import javax.swing.plaf.ColorUIResource

import at.iem.sysson
import de.sciss.desktop.impl.{SwingApplicationImpl, WindowHandlerImpl}
import de.sciss.desktop.{Menu, WindowHandler}
import de.sciss.mellite
import de.sciss.mellite.gui.LogFrame
import de.sciss.mellite.gui.impl.document.DocumentHandlerImpl
import de.sciss.mellite.{Application, Mellite, Prefs}

import scala.collection.immutable.{Seq => ISeq}
import scala.util.control.NonFatal

/** The main entry point for the desktop Swing application.
  * Please note that this should _not_ be the main class of the project,
  * but you should always invoke `at.iem.sysson.Main`, because it first
  * initializes some type extensions that would be missing otherwise.
  */
object SwingApplication extends SwingApplicationImpl("SysSon") with mellite.Application {
  override lazy val windowHandler: WindowHandler = new WindowHandlerImpl(this, menuFactory) {
    override lazy val usesInternalFrames = false // XXX TODO: eventually a preferences entry

    override def usesNativeDecoration: Boolean = Prefs.nativeWindowDecoration.getOrElse(true)
  }

  override def init(): Unit = {
    Locale.setDefault(Locale.US)    // (untested) make sure number formatting is consistent, while we do not have i18
    Application.init(this)

    //    showLog = true
    //    de.sciss.synth.proc.showLog           = true
    //    de.sciss.synth.proc.showAuralLog      = true
    //    de.sciss.synth.proc.showTransportLog  = true
    // de.sciss.synth.proc.impl.BounceImpl.DEBUG = true
    // UGenGraphBuilderLike.DEBUG = true
    // de.sciss.fscape.showStreamLog = true
    // de.sciss.lucre.matrix.impl.ReaderFactoryImpl.DEBUG = true

    // ---- look and feel ----

    try {
      Prefs.lookAndFeel.getOrElse(Prefs.LookAndFeel.default).install()
    } catch {
      case NonFatal(_) =>
    }

    // XXX TODO --- this should be fixed in Submin
    if (Mellite.isDarkSkin) {
      // for titled border
      UIManager.put("TitledBorder.titleColor" , new ColorUIResource(216, 220, 224))
      // for lucre matrix
      UIManager.put("Label.foreground"        , new ColorUIResource(216, 220, 224))
      UIManager.put("Label.disabledForeground", new ColorUIResource(new Color(216, 220, 224, 96)))
    }

    // ---- type extensions ----

    sysson.initTypes()
    sysson.gui.registerViews()

    LogFrame           .instance    // init

//    sys.addShutdownHook {
//      Stats.cacheDir.children { f =>
//        val n = f.name
//        n.startsWith("sysson") || n.endsWith(".cache")
//      } .foreach(_.delete())
//    }

    val melliteFrame = args.contains("--mellite-frame")

    if (melliteFrame)
      new mellite.gui.MainFrame
    else
      MainFrame()
  }

  protected def menuFactory: Menu.Root = MenuFactory.root

  override lazy val documentHandler: DocumentHandler = new DocumentHandlerImpl

  // ---- Application trait ----

  def topLevelObjects: ISeq[String] =
    List("Folder", "DataSource", "Sonification", "Plot")

  // XXX TODO --- can remove elements through preferences "novice user" switch
  lazy val objectFilter: String => Boolean = {
    case "Nuages" | "Recursion" => false
    case _ => true
  }

  def categSonification = "Sonification"
}
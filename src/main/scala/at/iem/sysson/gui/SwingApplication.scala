/*
 *  SwingApplication.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import java.util.Locale

import de.sciss.desktop.impl.{WindowHandlerImpl, SwingApplicationImpl}
import de.sciss.desktop.{WindowHandler, Menu}
import de.sciss.mellite
import de.sciss.mellite.gui.LogFrame
import language.existentials
import javax.swing.UIManager
import scala.util.control.NonFatal
import de.sciss.mellite.{Prefs, Application}
import com.alee.laf.checkbox.WebCheckBoxStyle
import com.alee.laf.progressbar.WebProgressBarStyle
import java.awt.Color
import at.iem.sysson

/** The main entry point for the desktop Swing application.
  * Please note that this should _not_ be the main class of the project,
  * but you should always invoke `at.iem.sysson.Main`, because it first
  * initializes some type extensions that would be missing otherwise.
  */
object SwingApplication extends SwingApplicationImpl("SysSon") {
  type Document = DocumentHandler.Document  // sysson.DataSourceLike

  override lazy val windowHandler: WindowHandler = new WindowHandlerImpl(this, menuFactory) {
    override lazy val usesInternalFrames = {
      false // XXX TODO: eventually a preferences entry
    }

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

    // ---- type extensions ----

    sysson.initTypes()
    sysson.gui.registerViews()

    // ---- look and feel ----

    try {
      val web = "com.alee.laf.WebLookAndFeel"
      UIManager.installLookAndFeel("Web Look And Feel", web)
      UIManager.setLookAndFeel(Prefs.lookAndFeel.getOrElse(Prefs.defaultLookAndFeel).getClassName)
    } catch {
      case NonFatal(_) =>
    }
    // work-around for web-laf bug #118
    new javax.swing.JSpinner
    // some custom web-laf settings
    WebCheckBoxStyle   .animated            = false
    WebProgressBarStyle.progressTopColor    = Color.lightGray
    WebProgressBarStyle.progressBottomColor = Color.gray
    // XXX TODO: how to really turn of animation?
    WebProgressBarStyle.highlightWhite      = new Color(255, 255, 255, 0)
    WebProgressBarStyle.highlightDarkWhite  = new Color(255, 255, 255, 0)

    LogFrame           .instance    // init
    DocumentHandler    .instance    // init
    DocumentViewHandler.instance    // init

    val mlltFrame = args.contains("--mellite-frame")

    if (mlltFrame)
      new mellite.gui.MainFrame
    else
      MainFrame()
  }

  protected def menuFactory: Menu.Root = MenuFactory.root
}
/*
 *  SonificationFrameImpl.scala
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
package impl

import at.iem.sysson.sound.Sonification
import de.sciss.desktop.Menu
import de.sciss.lucre.stm
import de.sciss.lucre.swing.{CellView, deferTx}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.AttrCellView
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.synth.proc.Workspace

object SonificationFrameImpl {
  def apply[S <: Sys[S]](obj: Sonification[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): SonificationFrame[S] = {
    val view  = SonificationView(obj)
    val name  = AttrCellView.name(obj)
    val res   = new Impl(view, name)
    res.init()
    deferTx {
      view.addListener {
        case SonificationView.Resized => res.pack()
      }
    }
    res
  }

  private final class Impl[S <: Sys[S]](val view: SonificationView[S], name: CellView[S#Tx, String])
    extends WindowImpl[S](name /* .map(n => s"$n : Sonification") */)
    with SonificationFrame[S] {

    override protected def initGUI(): Unit = {
      val Some(Menu.Item(it)) = window.handler.menuFactory.get("file.bounce")
      it.bind(window, view.actionBounce)
    }
  }
}

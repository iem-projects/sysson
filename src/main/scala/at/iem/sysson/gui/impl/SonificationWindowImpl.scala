/*
 *  SonificationWindowImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
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

import de.sciss.lucre.event.Sys
import at.iem.sysson.sound.Sonification
import de.sciss.synth.proc.Obj
import de.sciss.lucre.swing.deferTx

object SonificationWindowImpl {
  def apply[S <: Sys[S]](sonification: Obj.T[S, Sonification.Elem])
                        (implicit tx: S#Tx, workspace: Workspace[S]): SonificationWindow[S] = {
    val view  = SonificationView(sonification)
    val res   = new Impl(view)
    res.init()
    deferTx {
      view.addListener {
        case SonificationView.Resized => res.pack()
      }
    }
    res
  }

  private final class Impl[S <: Sys[S]](val view: SonificationView[S])
    extends WindowImpl[S](title0 = "Sonification Editor")
    with SonificationWindow[S] {

    // override protected def style = desktop.Window.Auxiliary
  }
}

/*
 *  GenViewFactory.scala
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
package fscape

import de.sciss.fscape.lucre.FScape.Output
import de.sciss.fscape.stream.Control
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.{GenContext, GenView}

object GenViewFactory {
  def apply(config: Control.Config = Control.Config()): GenView.Factory = new Impl

  def install(config: Control.Config = Control.Config()): Unit =
    GenView.addFactory(apply(config))

  private final class Impl extends GenView.Factory {
    def typeID: Int = Output.typeID

    type Repr[~ <: Sys[~]] = Output[~]

    def apply[S <: Sys[S]](obj: Output[S])(implicit tx: S#Tx, context: GenContext[S]): GenView[S] = ???
  }
}

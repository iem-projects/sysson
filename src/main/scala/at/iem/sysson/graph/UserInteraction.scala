/*
 *  UserInteraction.scala
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

package at.iem.sysson.graph

import de.sciss.synth.{HasSideEffect, Lazy}

/** Indicates a graph element that provides a user interface element. */
trait UserInteraction extends Lazy.Expander[Unit] with HasSideEffect {
  protected final def makeUGens: Unit = ()
}
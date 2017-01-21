/*
 *  Var.scala
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
package graph

import de.sciss.fscape.{GE, UGenGraph, UGenInLike}

object Var {
  final case class PlayLinear(variable: Var) extends GE.Lazy {
    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
      ???
    }
  }
}
final case class Var(name: String) extends GE.Lazy {
  protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
    ???
  }

  /** Unrolls all dimensions in time. */
  def playLinear(): Var.PlayLinear = Var.PlayLinear(this)
}

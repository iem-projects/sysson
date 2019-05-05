/*
 *  UserValue.scala
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

package at.iem.sysson.fscape.graph

import de.sciss.fscape.{GE, UGenGraph, UGenInLike}
import de.sciss.fscape.lucre.{UGenGraphBuilder => UGB}

object UserValue {
  final case class Key(name: String) extends UGB.Key {
    override def productPrefix = "UserValue.Key"
  }

  final case class Value(peer: Option[Double]) extends UGB.Value {
    override def productPrefix = "UserValue.Value"
  }
}
final case class UserValue(name: String, default: Double)
  extends GE.Lazy with UGB.Input {

  type Key    = UserValue.Key
  type Value  = UserValue.Value
  def key     = UserValue.Key(name)

  protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
    val ub    = UGB.get(b)
    val value = ub.requestInput(this).peer.getOrElse(default)
    value
  }
}
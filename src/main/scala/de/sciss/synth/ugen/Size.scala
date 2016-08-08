/*
 *  Size.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.synth
package ugen

case class Size(in: GE) extends GE.Lazy with ScalarRated {
  def makeUGens: UGenInLike = {
    val exp = in.expand.outputs
    exp.size
  }
}

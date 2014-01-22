/*
 *  LazyImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package graph
package impl

import de.sciss.synth.{UGenGraph, UGenInLike, GE}
import at.iem.sysson.sound.UGenGraphBuilder

trait LazyImpl extends GE.Lazy {
  protected final def makeUGens: UGenInLike =
    UGenGraph.builder match {
      case b: UGenGraphBuilder => makeUGens(b)
      case _ => sys.error("Expansion out of context")
    }

  protected def makeUGens(builder: UGenGraphBuilder): UGenInLike
}

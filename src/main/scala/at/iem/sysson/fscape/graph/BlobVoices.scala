/*
 *  BlobVoices.scala
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

import de.sciss.fscape.UGenSource.{expand, unwrap}
import de.sciss.fscape.graph.Blobs2D
import de.sciss.fscape.stream.{StreamIn, StreamOut}
import de.sciss.fscape.{GE, UGen, UGenGraph, UGenIn, UGenInLike, UGenSource, stream}

final case class BlobVoices(in: GE, width: GE, height: GE, blobs: Blobs2D, minWidth: GE, minHeight: GE, voices: GE = 4)
  extends UGenSource.SingleOut {

  protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike =
    unwrap(this, Vector(
      expand(in),
      expand(width)     , expand(height),
      expand(blobs.numBlobs),
      expand(blobs.xMin), expand(blobs.xMax),
      expand(blobs.yMin), expand(blobs.yMax),
      expand(minWidth)  , expand(minHeight),
      expand(voices)))

  protected def makeUGen(args: Vec[UGenIn])(implicit b: UGenGraph.Builder): UGenInLike =
    UGen.SingleOut(this, args)

  def makeStream(args: Vec[StreamIn])(implicit b: stream.Builder): StreamOut = {
    val Vec(in, width, height, numBlobs, xMin, xMax, yMin, yMax, minWidth, minHeight, voices) = args
    ??? // stream.BlobVoices(in = in.toDouble, width = width.toInt, height = height.toInt,
    //   numBlobs = numBlobs.toInt, xMin = xMin.toInt, xMax = xMax.toInt, yMin = yMin.toInt, yMax = yMax.toInt,
    //   minWidth = minWidth.toInt, minHeight = minHeight.toInt, voices = voices.toInt)
  }
}

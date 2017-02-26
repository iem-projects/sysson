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

/** A UGen that translates blob detection output into a continuous
  * signal suitable for writing as a managed-voices matrix.
  * The output will be two dimensional matrices with `width`
  * number of columns, and `voices * 10` number of rows
  * (that is, the `height` is replaced by the blob voices).
  * For each voice, the following ten values will be emitted
  * per column:
  *
  * - 0 - `id`
  * - 1 - `blobLeft`
  * - 2 - `blobTop`
  * - 3 - `blobWidth`
  * - 4 - `blobHeight`
  * - 5 - `boxTop`
  * - 6 - `boxHeight`
  * - 7 - `sliceMean`
  * - 8 - `sliceStdDev`
  * - 9 - `sliceCenter`
  *
  * Where `id` is zero for unused voice, and greater than zero for a valid blob.
  * During the existence of one blob, the blob boundary fields (1 to 4) remain
  * constant, and the box and slice values vary over time. Blobs are always
  * guaranteed to coherently occupying the same voice.
  *
  * @param in         the input matrices
  * @param width      the number of columns in the input matrices.
  *                   read as one element per matrix.
  * @param height     the number of rows in the input matrices.
  *                   read as one element per matrix.
  * @param blobs      the output from the blob detection
  * @param minWidth   the minimum blob width to be considered.
  *                   read as one element per matrix.
  * @param minHeight  the minimum blob height to be considered.
  *                   read as one element per matrix.
  * @param voices     the maximum number of parallel voices.
  *                   read at initialization time only.
  */
final case class BlobVoices(in: GE, width: GE, height: GE, blobs: Blobs2D, minWidth: GE = 0, minHeight: GE = 0,
                            voices: GE = 4)
  extends UGenSource.SingleOut {

  protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike =
    unwrap(this, Vector(
      expand(in),
      expand(width)     , expand(height),
      expand(minWidth)  , expand(minHeight),
      expand(voices),
      expand(blobs.numBlobs),
      expand(blobs.bounds),
      expand(blobs.numVertices),
      expand(blobs.vertices)
    ))

  protected def makeUGen(args: Vec[UGenIn])(implicit b: UGenGraph.Builder): UGenInLike =
    UGen.SingleOut(this, args)

  def makeStream(args: Vec[StreamIn])(implicit b: stream.Builder): StreamOut = {
    val Vec(in, width, height, minWidth, minHeight, voices, numBlobs, bounds, numVertices, vertices) = args
    stream.BlobVoices(in = in.toDouble, width = width.toInt, height = height.toInt,
      minWidth = minWidth.toInt, minHeight = minHeight.toInt, voices = voices.toInt,
      numBlobs = numBlobs.toInt, bounds = bounds.toDouble,
      numVertices = numVertices.toInt, vertices = vertices.toDouble
    )
  }
}

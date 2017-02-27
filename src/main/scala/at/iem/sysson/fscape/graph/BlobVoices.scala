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
import de.sciss.fscape.stream.{StreamIn, StreamOut}
import de.sciss.fscape.{GE, UGen, UGenGraph, UGenIn, UGenInLike, UGenSource, stream}

/** A UGen that translates blob detection output into a continuous
  * signal suitable for writing as a managed-voices matrix.
  * The output will be two dimensional matrices with the original
  * number of columns replaced by `voices * 10` number of columns.
  * (that is, for input matrix `[a][b]`, the output will be `[a][voices*10]`).
  *
  * For each voice, the following ten values will be emitted
  * per column (we give interpretation in 'time' and 'altitude' if
  * the input matrix is `[time][altitude]`, and [y][x] if the input
  * matrix is an image):
  *
  * - 0 - `id`          (zero for empty or no blob, greater than zero for voice id)
  * - 1 - `blobLeft`    (start altitude)
  * - 2 - `blobTop`     (start time)
  * - 3 - `blobWidth`   (altitude span)
  * - 4 - `blobHeight`  (time span)
  * - 5 - `boxLeft`     (start altitude of slice)
  * - 6 - `boxWidth`    (altitude span of slice)
  * - 7 - `sliceMean`
  * - 8 - `sliceStdDev`
  * - 9 - `sliceCenter`
  *
  * Where `id` is zero for unused voice, and greater than zero for a valid blob.
  * During the existence of one blob, the blob boundary fields (1 to 4) remain
  * constant, and the box and slice values vary over time. Blobs are always
  * guaranteed to coherently occupying the same voice.
  *
  * @param in           the input matrices
  * @param width        the number of columns in the input matrices.
  *                     read as one element per matrix.
  * @param height       the number of rows in the input matrices.
  *                     read as one element per matrix.
  * @param numBlobs     the `numBlobs` output from the blob detection
  * @param bounds       the `bounds` output from the blob detection
  * @param numVertices  the `numVertices` output from the blob detection
  * @param vertices     the `vertices` output from the blob detection
  * @param minWidth     the minimum blob width to be considered.
  *                     read as one element per matrix.
  * @param minHeight    the minimum blob height to be considered.
  *                     read as one element per matrix.
  * @param thresh       threshold in `in` for elements to be counted
  *                     in the slices
  * @param voices       the maximum number of parallel voices.
  *                     read at initialization time only.
  */
final case class BlobVoices(in: GE, width: GE, height: GE, numBlobs: GE, bounds: GE,
                            numVertices: GE, vertices: GE, minWidth: GE = 0, minHeight: GE = 0,
                            thresh: GE = 0.0, voices: GE = 4)
  extends UGenSource.SingleOut {

  protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike =
    unwrap(this, Vector(
      expand(in),
      expand(width)     , expand(height),
      expand(minWidth)  , expand(minHeight),
      expand(thresh)    , expand(voices),
      expand(numBlobs),
      expand(bounds),
      expand(numVertices),
      expand(vertices)
    ))

  protected def makeUGen(args: Vec[UGenIn])(implicit b: UGenGraph.Builder): UGenInLike =
    UGen.SingleOut(this, args)

  def makeStream(args: Vec[StreamIn])(implicit b: stream.Builder): StreamOut = {
    val Vec(in, width, height, minWidth, minHeight, thresh, voices, numBlobs, bounds, numVertices, vertices) = args
    stream.BlobVoices(in = in.toDouble, width = width.toInt, height = height.toInt,
      minWidth = minWidth.toInt, minHeight = minHeight.toInt, thresh = thresh.toDouble, voices = voices.toInt,
      numBlobs = numBlobs.toInt, bounds = bounds.toDouble,
      numVertices = numVertices.toInt, vertices = vertices.toDouble
    )
  }
}

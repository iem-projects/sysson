/*
 *  AudioFileCache.scala
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

package at.iem.sysson
package sound

import de.sciss.file._
import de.sciss.lucre.matrix
import de.sciss.lucre.matrix.{DataSource, Matrix}
import de.sciss.lucre.stm.{Sys, TxnLike}
import de.sciss.synth.proc.GenContext

import scala.concurrent.Future

/** The application wide cache for graphemes. */
object AudioFileCache {
  private val instance = {
    val config = matrix.AudioFileCache.Config()
    config.folder = dataDir / "cache" // XXX TODO should read prefs
    matrix.AudioFileCache(config)
  }

  type Value = matrix.AudioFileCache.Value

  def acquire[S <: Sys[S]](factory: Matrix.ReaderFactory[S])(implicit tx: S#Tx, resolver: DataSource.Resolver[S],
                                                             context: GenContext[S]): Future[Value] =
    instance.acquire(factory)

  def release(key: Matrix.Key)(implicit tx: TxnLike): Unit = instance.release(key)
}

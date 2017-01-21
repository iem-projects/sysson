/*
 *  WorldMapOverlay.scala
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
package gui

import java.awt.Shape
import java.awt.geom.GeneralPath
import java.io.{DataInputStream, FileNotFoundException}

import scala.concurrent.{ExecutionContext, Future, blocking}

/** A world map overlay shape with the normalized width of 360 and height of 180.
  * It is read from a binary file that was created from the following
  * public domain image:
  *
  * https://commons.wikimedia.org/wiki/File:BlankMap-Equirectangular.svg
  *
  * The binary file contains a sub-sampled union shape of this original SVG file.
  */
object WorldMapOverlay {
  def apply(): Future[Shape] = instance

  private lazy val instance: Future[Shape] = {
    import ExecutionContext.Implicits.global
    Future {
      blocking {
        val path = "BlankMap-Equirectangular.dat"
        val is = Main.getClass.getResourceAsStream(path)
        if (is == null) throw new FileNotFoundException(path)
        try {
          val gp  = new GeneralPath()
          val dis = new DataInputStream(is)
          var num = dis.readShort().toInt
          while (num > 0) {
            var sz = dis.readShort().toInt - 1
            val x0 = dis.readFloat()
            val y0 = dis.readFloat()
            gp.moveTo(x0, y0)
            while (sz > 0) {
              val x = dis.readFloat()
              val y = dis.readFloat()
              gp.lineTo(x, y)
              sz -= 1
            }
            gp.closePath()
            num -= 1
          }
          gp

        } finally {
          is.close()
        }
      }
    }
  }
}
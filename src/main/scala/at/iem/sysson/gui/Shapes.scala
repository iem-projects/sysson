/*
 *  Shapes.scala
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
package gui

import java.awt.geom.Path2D

object Shapes {
  def Spreadsheet(p: Path2D): Unit = {
    p.moveTo(4.0f, 4.0f)
    p.lineTo(4.0f, 10.3125f)
    p.lineTo(10.3125f, 10.3125f)
    p.lineTo(10.3125f, 4.0f)
    p.lineTo(4.0f, 4.0f)
    p.moveTo(12.84375f, 4.0f)
    p.lineTo(12.84375f, 10.3125f)
    p.lineTo(19.15625f, 10.3125f)
    p.lineTo(19.15625f, 4.0f)
    p.lineTo(12.84375f, 4.0f)
    p.moveTo(21.6875f, 4.0f)
    p.lineTo(21.6875f, 10.3125f)
    p.lineTo(28.0f, 10.3125f)
    p.lineTo(28.0f, 4.0f)
    p.lineTo(21.6875f, 4.0f)
    p.moveTo(4.0f, 12.84375f)
    p.lineTo(4.0f, 19.15625f)
    p.lineTo(10.3125f, 19.15625f)
    p.lineTo(10.3125f, 12.84375f)
    p.lineTo(4.0f, 12.84375f)
    p.moveTo(12.84375f, 12.84375f)
    p.lineTo(12.84375f, 19.15625f)
    p.lineTo(19.15625f, 19.15625f)
    p.lineTo(19.15625f, 12.84375f)
    p.lineTo(12.84375f, 12.84375f)
    p.moveTo(21.6875f, 12.84375f)
    p.lineTo(21.6875f, 19.15625f)
    p.lineTo(28.0f, 19.15625f)
    p.lineTo(28.0f, 12.84375f)
    p.lineTo(21.6875f, 12.84375f)
    p.moveTo(4.0f, 21.6875f)
    p.lineTo(4.0f, 28.0f)
    p.lineTo(10.3125f, 28.0f)
    p.lineTo(10.3125f, 21.6875f)
    p.lineTo(4.0f, 21.6875f)
    p.moveTo(12.84375f, 21.6875f)
    p.lineTo(12.84375f, 28.0f)
    p.lineTo(19.15625f, 28.0f)
    p.lineTo(19.15625f, 21.6875f)
    p.lineTo(12.84375f, 21.6875f)
    p.moveTo(21.6875f, 21.6875f)
    p.lineTo(21.6875f, 28.0f)
    p.lineTo(28.0f, 28.0f)
    p.lineTo(28.0f, 21.6875f)
    p.lineTo(21.6875f, 21.6875f)
  }
}

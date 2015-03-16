/*
 *  ColorPaletteTableImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.gui
package impl

import at.iem.sysson.Main
import at.iem.sysson.gui.ColorPaletteTable.Segment
import de.sciss.serial.{ImmutableSerializer, DataInput, DataOutput}

object ColorPaletteTableImpl {
  def seq(name: String, values: Array[Int], background: Int, foreground: Int): ColorPaletteTable =
    new SeqImpl(name, values, background = background, foreground = foreground)

  def segments(name: String, values: Array[Segment], background: Int, foreground: Int, fill: Int): ColorPaletteTable =
    new SegmImpl(name, segments = values, background = background, foreground = foreground, fill = fill)

  def read(in: DataInput): ColorPaletteTable = {
    val ver = in.readInt()
    if (ver != SER_VERSION) sys.error(s"Unexpected cookie (found $ver, expected $SER_VERSION)")
    val name = in.readUTF()
    in.readByte() match {
      case 0 =>
        val num         = in.readInt()
        val values      = Array.fill(num)(in.readInt())
        val background  = in.readInt()
        val foreground  = in.readInt()
        new SeqImpl(name, values, background = background, foreground = foreground)

      case 1 =>
        val num         = in.readInt()
        val segm        = Array.fill(num)(Segment.read(in))
        val background  = in.readInt()
        val foreground  = in.readInt()
        val fill        = in.readInt()
        new SegmImpl(name, segm, background = background, foreground = foreground, fill = fill)

      case other => sys.error(s"Unexpected palette type $other")
    }
  }

  lazy val builtIn: Map[String, ColorPaletteTable] = {
    val mapSer  = implicitly[ImmutableSerializer[Map[String, ColorPaletteTable]]]
    val is      = Main.getClass.getResourceAsStream("color-tables.bin")
    val bytes   = new Array[Byte](is.available())
    is.read(bytes)
    is.close()
    val in      = DataInput(bytes)
    mapSer.read(in)
  }

  private final val SER_VERSION = 0x43505400  // "CPT\0"

  private trait BasicImpl extends ColorPaletteTable {

    override def toString = s"ColorPaletteTable($name, num = $num)"

    final def get(value: Double): Int =
      if (value < minValue) background
      else if (value >= maxValue) foreground
      else if (java.lang.Double.isNaN(value)) fill
      else getNoCheck(value)

    def write(out: DataOutput): Unit = {
      out.writeInt(SER_VERSION)
      out.writeUTF(name)
      writeData(out)
    }

    protected def getNoCheck(value: Double): Int

    protected def writeData(out: DataOutput): Unit
  }

  private final class SeqImpl(val name: String, values: Array[Int],
                              val background: Int, val foreground: Int)
    extends BasicImpl {

    def fill: Int = 0x7F7F7F

    def minValue: Double = 0
    def maxValue: Double = values.length

    def num: Int = values.length

    def apply(idx: Int): Segment = {
      val colr = values(idx)
      Segment(lowValue = idx, lowColor = colr, highValue = idx + 1, highColor = colr)
    }

    def getNoCheck(value: Double): Int = values(value.toInt)

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(0)
      out.writeInt(values.length)
      values.foreach(out.writeInt)
      out.writeInt(background)
      out.writeInt(foreground)
    }
  }

  private final class SegmImpl(val name: String, segments: Array[Segment], val background: Int,
                               val foreground: Int, val fill: Int)
    extends BasicImpl {

    private val startValues = segments.map(_.lowValue)

    def minValue: Double = segments(0).lowValue
    def maxValue: Double = segments(segments.length - 1).highValue

    def num: Int = segments.length

    def apply(idx: Int): Segment = segments(idx)

    def getNoCheck(value: Double): Int = {
      val idx0 = java.util.Arrays.binarySearch(startValues, value)
      val idx  = math.abs(idx0) // if (idx0 >= 0) idx0 else -idx0 + 1
      segments(idx).apply(value)
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(1)
      out.writeInt(segments.length)
      segments.foreach(_.write(out))
      out.writeInt(background)
      out.writeInt(foreground)
      out.writeInt(fill      )
    }
  }
}

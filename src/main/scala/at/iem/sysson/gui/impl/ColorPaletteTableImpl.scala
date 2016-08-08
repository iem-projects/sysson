/*
 *  ColorPaletteTableImpl.scala
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

package at.iem.sysson.gui
package impl

import at.iem.sysson.Main
import at.iem.sysson.gui.ColorPaletteTable.Segment
import de.sciss.serial.{ImmutableSerializer, DataInput, DataOutput}

object ColorPaletteTableImpl {
  def seq(name: String, values: Array[Int], background: Int, foreground: Int, fill: Int): ColorPaletteTable =
    new SeqImpl(name, values, background = background, foreground = foreground, fill = fill)

  def segments(name: String, values: Array[Segment], background: Int, foreground: Int, fill: Int): ColorPaletteTable = {
    val isDiscrete = values.forall(s => s.lowColor == s.highColor)
    new SegmImpl(name, segments = values, background = background, foreground = foreground, fill = fill,
      isDiscrete = isDiscrete)
  }

  def read(in: DataInput): ColorPaletteTable = {
    val ver = in.readInt()
    if (ver != SER_VERSION) sys.error(s"Unexpected cookie (found $ver, expected $SER_VERSION)")
    val name        = in.readUTF()
    val background  = in.readInt()
    val foreground  = in.readInt()
    val fill        = in.readInt()

    val tpe = in.readByte()
    if (tpe == 0) {
      val num = in.readInt()
      val values = Array.fill(num)(in.readInt())
      new SeqImpl(name, values, background = background, foreground = foreground, fill = fill)
    } else {
      if (tpe != 1 && tpe != 2) sys.error(s"Unexpected palette type $tpe")
      val isDiscrete  = tpe == 1
      val num         = in.readInt()
      val segm        = Array.fill(num)(if (isDiscrete) readDiscreteSegment(in) else readGradientSegment(in))
      new SegmImpl(name, segm, background = background, foreground = foreground, fill = fill, isDiscrete = isDiscrete)
    }
  }

  lazy val builtIn: Map[String, ColorPaletteTable] = {
    val mapSer  = implicitly[ImmutableSerializer[Map[String, ColorPaletteTable]]]
    val is      = Main.getClass.getResourceAsStream("color-tables.bin")
    val bytes   = new Array[Byte](is.available())

    // NOTE: this shit doesn't work, apparently `read(array)` doesn't return the full amount
    // if the file is too large (> 20 KB).
    //
    //    val bytes   = new Array[Byte](is.available())
    //    is.read(bytes)
    var off = 0
    do {
      val read = is.read(bytes, off, bytes.length - off)
      off += read
    } while (off < bytes.length)
    is.close()
    // val bb = java.nio.ByteBuffer.wrap(bytes)
    // de.sciss.osc.Packet.printHexOn(bb, System.out)

    val in = DataInput(bytes)
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

    final def indexOf(value: Double): Int =
      if (value < minValue) -1
      else if (value >= maxValue) num
      else if (java.lang.Double.isNaN(value)) throw new IllegalArgumentException("NaN")
      else indexOfNoCheck(value)

    final def write(out: DataOutput): Unit = {
      out.writeInt(SER_VERSION)
      out.writeUTF(name)
      out.writeInt(background)
      out.writeInt(foreground)
      out.writeInt(fill)
      writeData(out)
    }

    protected def getNoCheck(value: Double): Int

    protected def indexOfNoCheck(value: Double): Int

    protected def writeData(out: DataOutput): Unit
  }

  private final class SeqImpl(val name: String, values: Array[Int],
                              val background: Int, val foreground: Int, val fill: Int)
    extends BasicImpl {

    def isDiscrete = true

    def minValue: Double = 0
    def maxValue: Double = values.length

    def num: Int = values.length

    def apply(idx: Int): Segment = {
      val colr = values(idx)
      Segment(lowValue = idx, lowColor = colr, highValue = idx + 1, highColor = colr)
    }

    protected def getNoCheck(value: Double): Int = values(value.toInt)

    protected def indexOfNoCheck(value: Double): Int = value.toInt

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(0)
      out.writeInt(values.length)
      values.foreach(out.writeInt)
    }
  }

  private final class SegmImpl(val name: String, protected val segments: Array[Segment], val background: Int,
                               val foreground: Int, val fill: Int, val isDiscrete: Boolean)
    extends BasicImpl {

    private val startValues = segments.map(_.lowValue)

    def minValue: Double = segments(0).lowValue
    def maxValue: Double = segments(segments.length - 1).highValue

    def num: Int = segments.length

    def apply(idx: Int): Segment = segments(idx)

    def getNoCheck(value: Double): Int = {
      val idx = indexOfNoCheck(value)
      segments(idx).apply(value)
    }

    protected def indexOfNoCheck(value: Double): Int = {
      val idx0 = java.util.Arrays.binarySearch(startValues, value)
      if (idx0 < 0) -(idx0 + 1) - 1 else idx0
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(if (isDiscrete) 1 else 2)
      out.writeInt(segments.length)
      segments.foreach(s => if (isDiscrete) writeDiscreteSegment(s, out) else writeGradientSegment(s, out))
    }
  }

  private def readGradientSegment(in: DataInput): Segment = {
    val lowValue  = in.readDouble()
    val highValue = in.readDouble()
    val lowColor  = in.readInt   ()
    val highColor = in.readInt   ()
    Segment(lowValue = lowValue, lowColor = lowColor, highValue = highValue, highColor = highColor)
  }

  private def writeGradientSegment(s: Segment, out: DataOutput): Unit = {
    out.writeDouble(s.lowValue )
    out.writeDouble(s.highValue)
    out.writeInt   (s.lowColor )
    out.writeInt   (s.highColor)
  }

  private def readDiscreteSegment(in: DataInput): Segment = {
    val lowValue  = in.readDouble()
    val highValue = in.readDouble()
    val color     = in.readInt   ()
    Segment(lowValue = lowValue, lowColor = color, highValue = highValue, highColor = color)
  }

  private def writeDiscreteSegment(s: Segment, out: DataOutput): Unit = {
    out.writeDouble(s.lowValue )
    out.writeDouble(s.highValue)
    out.writeInt   (s.lowColor )
  }
}
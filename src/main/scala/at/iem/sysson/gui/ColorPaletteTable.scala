/*
 *  ColorPaletteTable.scala
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

import java.awt.{LinearGradientPaint, Color}
import java.io.{FileInputStream, InputStream}

import at.iem.sysson.gui.impl.{ColorPaletteTableImpl => Impl}
import de.sciss.file._
import de.sciss.serial.{ImmutableSerializer, DataInput, DataOutput, Writable}

object ColorPaletteTable {
  def read(file: File): ColorPaletteTable = {
    lazy val source = new FileInputStream(file)
    val (name, ext) = file.baseAndExt
    val extL = ext.toLowerCase
    extL match {
      case "act" => readACT(name, source)
      case "cpt" => readCPT(name, source)
      case "rgb" => readRGB(name, source)
      case _ => throw new UnsupportedOperationException(s"File extension '$ext' not supported")
    }
  }

  /** Reads a color table from the source, assuming `.cpt` format. Closes the source before returning. */
  def readCPT(name: String, source: InputStream): ColorPaletteTable = try {
    val lines   = activeLines(source)
    // start-value r256 g256 b256 end-value r256 g256 b256
    val PatColr = raw"""(\-?\d+(?:\.\d+)?)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\-?\d+(?:\.\d+)?)\s+(\d+)\s+(\d+)\s+(\d+)""".r
    val PatBg   = raw"""B\s+(\d+)\s+(\d+)\s+(\d+)""".r
    val PatFg   = raw"""F\s+(\d+)\s+(\d+)\s+(\d+)""".r
    val PatFill = raw"""N\s+(\d+)\s+(\d+)\s+(\d+)""".r

    var background  = -1
    var foreground  = -1
    var fill        = -1
    val segmB       = Array.newBuilder[Segment]

    def mkRGB(rs: String, gs: String, bs: String): Int = (rs.toInt << 16) | (gs.toInt << 8) | bs.toInt

    lines.foreach {
      case PatColr(v0s, r0s, g0s, b0s, v1s, r1s, g1s, b1s) =>
        segmB += Segment(lowValue  = v0s.toDouble, lowColor  = mkRGB(r0s, g0s, b0s),
                         highValue = v1s.toDouble, highColor = mkRGB(r1s, g1s, b1s))
      case PatBg  (rs, gs, bs) => background = mkRGB(rs, gs, bs)
      case PatFg  (rs, gs, bs) => foreground = mkRGB(rs, gs, bs)
      case PatFill(rs, gs, bs) => fill       = mkRGB(rs, gs, bs)
    }

    val segm = segmB.result()
    if (background == -1) background = segm(0).lowColor
    if (foreground == -1) foreground = segm(segm.length - 1).highColor
    if (fill       == -1) fill       = 0x7F7F7F

    Impl.segments(name, values = segm, background = background, foreground = foreground, fill = fill)

  } finally {
    source.close()
  }

  /** Reads a color table from the source, assuming `.act` format. Closes the source before returning. */
  def readACT(name: String, source: InputStream): ColorPaletteTable = try {
    val buf = new Array[Byte](4)

    def nextRGB(): Int = {
      source.read(buf, 0, 3)
      ((buf(0) & 0xFF) << 16) | ((buf(1) & 0xFF) << 8) | (buf(2) & 0xFF)
    }

    val values0 = Array.fill(256)(nextRGB())
    val values = if (source.available() >= 2) {
      source.read(buf, 0, 2)
      val num = ((buf(0) & 0xFF) << 8) | (buf(1) & 0xFF)
      require (num >= 0 && num <= 256, s"number of entries is $num")
      if (num == 0) values0 else values0.take(num)  // idiotic convention
    } else values0

    if (source.available() >= 2) source.read(buf, 0, 2) // XXX TODO -- what is this?

    val background  = if (source.available() >= 3) nextRGB() else values(0)
    val foreground  = if (source.available() >= 3) nextRGB() else values(values.length - 1)
    val fill        = 0x7F7F7F

    Impl.seq(name = name, values = values, background = background, foreground = foreground, fill = fill)

  } finally {
    source.close()
  }

  /** Reads a color table from the source, assuming `.rgb` format. Closes the source before returning. */
  def readRGB(name: String, source: InputStream): ColorPaletteTable = try {
    val lines   = activeLines(source)
    val PatNum  = raw"""ncolor\s*=\s*(\d+)""".r
    val PatRGB  = raw"""(\d+(?:\.\d+)?)\s+(\d+(?:\.\d+)?)\s+(\d+(?:\.\d+)?)""".r
    val PatNum(numS) = lines.next()
    val num = numS.toInt
    val values: Array[Int] = lines.take(num).map { line =>
      val PatRGB(r, g, b) = line
      val rgb = (to256(r.toDouble) << 16) | (to256(g.toDouble) << 8) | to256(b.toDouble)
      rgb
    } .toArray
    val fill = 0x7F7F7F
    Impl.seq(name = name, values = values, background = values(0), foreground = values(values.length - 1), fill = fill)

  } finally {
    source.close()
  }

  private def to256(in: Double): Int = {
    import de.sciss.numbers.Implicits._
    in.clip(0, 1).linlin(0, 1, 0x00, 0xFF).toInt
  }

  private def activeLines(in: InputStream): Iterator[String] = {
    val text = io.Source.fromInputStream(in, "UTF-8")
    text.getLines().map(_.trim).filterNot(ln => ln.isEmpty || ln.startsWith("#"))
  }

  /** A color table interval.
    *
    * @param lowValue   interval scale lower bound (inclusive)
    * @param lowColor   RGB-Int color corresponding to lower bound
    * @param highValue  interval scale upper bound (exclusive)
    * @param highColor  RGB-Int color corresponding to upper bound
    */
  final case class Segment(lowValue: Double, lowColor: Int, highValue: Double, highColor: Int) {

    override def toString = {
      val lowHex  = (lowColor  + 0x1000000).toHexString.substring(1)
      val highHex = (highColor + 0x1000000).toHexString.substring(1)
      s"$productPrefix(lowValue = $lowValue, lowColor = 0x$lowHex, highValue = $highValue, highColor = 0x$highHex"
    }

    /** Interpolates a value that lies inside the interval of this segment. */
    def apply(value: Double): Int =
      if (lowColor == highColor || value <= lowValue) lowColor
      else if (value >= highValue) highColor
      else {
        val w2 = (value - lowValue) / (highValue - lowValue)
        val w1 = 1.0 - w2
        val r1 = (lowColor  & 0xFF0000) >> 16
        val g1 = (lowColor  & 0x00FF00) >>  8
        val b1 =  lowColor  & 0x0000FF
        val r2 = (highColor & 0xFF0000) >> 16
        val g2 = (highColor & 0x00FF00) >>  8
        val b2 =  highColor & 0x0000FF
        val r  = (r1 * w1 + r2 * w2 + 0.5).toInt
        val g  = (g1 * w1 + g2 * w2 + 0.5).toInt
        val b  = (b1 * w1 + b2 * w2 + 0.5).toInt
        (r << 16) | (g << 8) | b
      }
  }

  implicit object serializer extends ImmutableSerializer[ColorPaletteTable] {
    def write(cpt: ColorPaletteTable, out: DataOutput): Unit = cpt.write(out)

    def read(in: DataInput): ColorPaletteTable = Impl.read(in)
  }

  def builtIn: Map[String, ColorPaletteTable] = Impl.builtIn

  // ---- display ----

  /** Constructs a Swing `Paint` object from a palette, painting the entire scale.  */
  def toPaint(palette: ColorPaletteTable, startX: Float = 0f, startY: Float = 0f,
                                          endX  : Float = 1f, endY  : Float = 0f): LinearGradientPaint = {
    var i         = 0
    val num       = palette.num
    val fractions = new Array[Float](num << 1)
    val colors    = new Array[Color](num << 1)
    val pLow      = palette.minValue
    val pHigh     = palette.maxValue
    val pRange    = pHigh - pLow
    while (i < num) {
      palette.minValue
      val segm = palette(i)
      val f1 = (segm.lowValue  - pLow) / pRange
      val f2 = (segm.highValue - pLow) / pRange
      var j  = i << 1
      fractions(j) = f1.toFloat
      colors   (j) = new Color(segm.lowColor)
      j += 1
      fractions(j) = f2.toFloat - 1.0e-3f
      colors   (j) = new Color(segm.highColor)
      i += 1
    }
    new LinearGradientPaint(startX, startY, endX, endY, fractions, colors)
  }
}
trait ColorPaletteTable extends Writable {
  def name: String

  /** RGB value for values below scale lower bound. */
  def background: Int

  /** RGB value for values above scale upper bound. */
  def foreground: Int

  /** RGB value for missing values. */
  def fill: Int

  /** Number of segments. */
  def num: Int

  /** Scale lower bound. */
  def minValue: Double

  /** Scale upper bound. */
  def maxValue: Double

  def apply(idx: Int): ColorPaletteTable.Segment

  /** Calculates and interpolates the color for a given value */
  def get(value: Double): Int

  def indexOf(value: Double): Int

  def isDiscrete: Boolean
}
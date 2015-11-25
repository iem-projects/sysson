package at.iem.sysson

import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import at.iem.sysson.gui.ColorPaletteTable
import de.sciss.file._

object MkColorPaletteImages extends App {
  val dirPath = args.headOption.getOrElse(sys.error("Must call with one argument, the output directory"))
  val dir     = file(dirPath)
  require(dir.isDirectory, s"Argument '$dir' must be the path of an existing directory")

  val width   = 256
  val height  = 32

  println(
    """||color scale|name|
       ||-----------|----|""".stripMargin)
  ColorPaletteTable.builtIn.values.toSeq.sortBy(_.name.toUpperCase).foreach { p =>
    import p.name
    val paint = ColorPaletteTable.toPaint(p, endX = width)
    val out   = dir / s"cpt-$name.png"
    val img   = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g     = img.createGraphics()
    g.setPaint(paint)
    g.fillRect(0, 0, width, height)
    g.dispose()
    ImageIO.write(img, "png", out)
    println(s"""|![$name](Icons/${out.name})|$name|""")
  }
}

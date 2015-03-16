package at.iem.sysson

import at.iem.sysson.gui.ColorPaletteTable
import de.sciss.file._
import de.sciss.serial.{DataOutput, ImmutableSerializer}

import scala.collection.breakOut

// args: <input-dir> <output-file>
object MakeColorTables extends App {
  val outputFile = file(if (args.length > 1) args(1) else "src/main/resources/at/iem/sysson/color-tables.bin")

  if (outputFile.exists()) {
    println(s"Output file '$outputFile' already exists.")
    println("Existing tables:")
    ColorPaletteTable.builtIn.keys.foreach(println)

  } else {
    val inputDir = file(args(0))
    val inputFiles = inputDir.children { f => val ext = f.ext.toLowerCase; ext == "act" || ext == "cpt" || ext == "rgb" }
      .sortBy(_.name.toLowerCase)

    val palettes: Map[String, ColorPaletteTable] = inputFiles.map { inF =>
      println(s"Reading '${inF.name}'...")
      val pal = ColorPaletteTable.read(inF)
      println(pal)
      pal.name -> pal
    } (breakOut)

    val mapSer = implicitly[ImmutableSerializer[Map[String, ColorPaletteTable]]]
    outputFile.parentOption.foreach(_.mkdirs())
    val out = DataOutput.open(outputFile)
    mapSer.write(palettes, out)
    out.close()
  }
}
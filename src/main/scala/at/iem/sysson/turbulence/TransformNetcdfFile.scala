/*
 *  TransformNetcdfFile.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package turbulence

import de.sciss.file._
import ucar.{ma2, nc2}
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.JavaConverters._
import Implicits._

object TransformNetcdfFile {
  def main(args: Array[String]): Unit = {
    val inF = userHome / "IEM" / "SysSon" / "Data" / "201211" / "gcm" / "New_Deutschlandradio_MPI_M" /
      "tas" / "ZON_tas_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"
    val in = openFile(inF)
    try {
      val out = userHome / "Documents" / "temp" / "test.nc"
      apply(in, out, "tas", Vec.empty, Vec.empty)(identity)
    } finally {
      in.close()
    }
  }

  // cf. http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/tutorial/NetcdfWriting.html
  def apply(in: nc2.NetcdfFile, out: File, varName: String,
            inDims: Vec[String], outDims: Vec[(String, ma2.Array)])(fun: Vec[Vec[Float]] => Vec[Vec[Float]]): Unit = {
    val location  = out.path
    val writer    = nc2.NetcdfFileWriter.createNew(nc2.NetcdfFileWriter.Version.netcdf3, location, null)

    val inVar     = in.variableMap(varName)
    val allInDims = inVar.dimensions
    val (alterInDims, keepInDims) = allInDims.partition(d => inDims.contains(d.name))

    val (keepOutDims, keepOutDimsV) = keepInDims.map { inDim =>
      val outDim  = writer.addDimension(null, inDim.name, inDim.size)
      val inVar   = in.variableMap(inDim.name)
      //      val dimData = inVar.read()
      val outVarD = writer.addVariable(null, inVar.getShortName, inVar.dataType, Seq(outDim).asJava)
      //      writer.write(outVarD, dimData)
      (outDim, outVarD)
    } .unzip

    val (alterOutDims, alterOutDimsV) = outDims.map { case (dimName, dimData) =>
      val outDim  = writer.addDimension(null, dimName, dimData.size.toInt)
      val dt      = ma2.DataType.getType(dimData.getElementType)
      val outVarD = writer.addVariable(null, dimName, dt, Seq(outDim).asJava)
      //      writer.write(outVarD, dimData)
      (outDim, outVarD)
    } .unzip

    val outVar = writer.addVariable(null, inVar.getShortName, inVar.getDataType, (keepOutDims ++ alterOutDims).asJava)

  //    val latDim    = writer.addDimension(null, "lat", 13 /*  64 */)
  //    val lonDim    = writer.addDimension(null, "lon", 21 /* 128 */)
  //    val dims      = new ju.ArrayList[nc2.Dimension]()
  //    dims.add(latDim)
  //    dims.add(lonDim)
  //    // use float instead of double, because sysson plot in previous version restricted to float
  //    val t         = writer.addVariable(null, "temperature", ma2.DataType.FLOAT /* DOUBLE */, dims)
  //    t.addAttribute(new nc2.Attribute("units", "K"))
  //    val data      = ma2.Array.factory(classOf[Int], Array(3), Array(1, 2, 3))
  //    t.addAttribute(new nc2.Attribute("scale", data))
  //    // add a string-valued variable: char svar(80)
  //    /* val sVarLen = */ writer.addDimension(null, "svar_len", 80)
  //    writer.addVariable(null, "svar", ma2.DataType.CHAR, "svar_len")
  //    // add a 2D string-valued variable: char names(names, 80)
  //    /* val names = */ writer.addDimension(null, "names", 3)
  //    writer.addVariable(null, "names", ma2.DataType.CHAR, "names svar_len")
  //    // add a scalar variable
  //    writer.addVariable(null, "scalar", ma2.DataType.DOUBLE, new ju.ArrayList[nc2.Dimension]())
  //    // add global attributes
  //    writer.addGroupAttribute(null, new nc2.Attribute("yo", "face"))
  //    writer.addGroupAttribute(null, new nc2.Attribute("versionD", 1.2))
  //    writer.addGroupAttribute(null, new nc2.Attribute("versionF", 1.2f))
  //    writer.addGroupAttribute(null, new nc2.Attribute("versionI", 1))
  //    writer.addGroupAttribute(null, new nc2.Attribute("versionS", 2.toShort))
  //    writer.addGroupAttribute(null, new nc2.Attribute("versionB", 3.toByte))

    // create the file; ends "define mode"
    writer.create()

    (keepInDims zip keepOutDimsV).foreach { case (inDim, outDimV) =>
      val inVar   = in.variableMap(inDim.name)
      val dimData = inVar.read()
      // val outVarD = writer.addVariable(null, inVar.getShortName, inVar.dataType, Seq(outDim).asJava)
      writer.write(outDimV, dimData)
    }

    (outDims zip alterOutDimsV).foreach { case ((dimName, dimData), outVarD) =>
      // val dt      = ma2.DataType.getType(dimData.getElementType)
      // val outVarD = writer.addVariable(null, dimName, dt, Seq(outDim).asJava)
      writer.write(outVarD, dimData)
    }

    writer.write()



  //    // write data to variable
  //    val v = writer.findVariable("temperature")
  //    val shape = v.getShape
  //    val A = new ma2.ArrayFloat /*  ArrayDouble */ .D2(shape(0), shape(1))
  //    val ima = A.getIndex
  //    for (i <- 0 until shape(0)) {
  //      for (j <- 0 until shape(1)) {
  //        A.setFloat /* .setDouble */ (ima.set(i, j), (i * 100 + j).toFloat /* .toDouble */)
  //      }
  //    }
  //    val origin = new Array[Int](2)
  //    writer.write(v, origin, A)

    writer.close()
  }
}

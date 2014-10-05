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
import Implicits._

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.JavaConverters._
import scala.collection.breakOut
import scala.language.implicitConversions

object TransformNetcdfFile {
  def main(args: Array[String]): Unit = {
    val inF = userHome / "IEM" / "SysSon" / "Data" / "201211" / "gcm" / "New_Deutschlandradio_MPI_M" /
      "tas" / "ZON_tas_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"
    val in = openFile(inF)
    try {
      val out = userHome / "Documents" / "temp" / "test.nc"
      val spkData = ma2.Array.factory(Turbulence.Channels.map(_.num)(breakOut): Array[Int])
      apply(in, out, "tas", Vec("lat", "lon"), Vec(Keep("time"), Create("spk", spkData))) { case (origin, arr) =>
        val dIn0  = arr.copyToNDJavaArray().asInstanceOf[Array[Array[Float]]]
        val dIn   = dIn0.flatten
        val dOut = Array.tabulate(Turbulence.Channels.size) { i =>
          (i + origin(0)).toFloat * dIn(i % dIn.length)
        }
        ma2.Array.factory(dOut)
      }
    } finally {
      in.close()
    }
  }

  object OutDim {
    implicit def byName(name: String): OutDim = Keep(name)
  }
  sealed trait OutDim { def isCopy: Boolean }
  final case class Keep(name: String) extends OutDim { def isCopy = true }
  object Create { def apply(name: String, values: ma2.Array): Create = new Create(name, values) }
  final class Create(val name: String, val values: ma2.Array) extends OutDim { def isCopy = false }

  // def deinterleave[A](flat: Vec[A], shape: Vec[Int]): Vec[Vec[A]] = ...

  // cf. http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/tutorial/NetcdfWriting.html
  /** Transform an input NetCDF file into an output NetCDF file, by copying a given
    * variable and applying an optional transform to the data.
    *
    * '''Note:''' This is not optimized for speed, yet.
    *
    * @param in         the input file to transform
    * @param out        the file to which the output will be written
    * @param varName    the variable to copy/transform
    * @param inDims     the dimensions of the variable to transform. these will be removed
    *                   from the target variable
    * @param outDimsSpec the dimensions of the output variable. each spec can either
    *                   indicate a verbatim copy (`Keep`) or the result of the transformation (`Create`)
    * @param fun        a function that will transform the variable's data matrix. It is passed the origin
    *                   in the kept dimensions (origin of the output shape minus the created dimensions)
    *                   and an object
    *                   of dimension `inDims.size` and is required to output an object of
    *                   dimension `outDims.filterNot(_.isCopy).size`. The dimensions are sorted to
    *                   correspond with `inDims`. The function is called repeatedly, iterating
    *                   over all other input dimensions except those in `inDims`.
    */
  def apply(in: nc2.NetcdfFile, out: File, varName: String,
            inDims: Vec[String], outDimsSpec: Vec[OutDim])(fun: (Vec[Int], ma2.Array) => ma2.Array): Unit = {
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

    val (alterOutDims, alterOutDimsD, alterOutDimsV) = outDimsSpec.collect {
      case c: Create =>
        val outDim  = writer.addDimension(null, c.name, c.values.size.toInt)
        val dt      = ma2.DataType.getType(c.values.getElementType)
        val outVarD = writer.addVariable(null, c.name, dt, Seq(outDim).asJava)
        //      writer.write(outVarD, dimData)
        (outDim, c.values, outVarD)
    } .unzip3

    val outDims = outDimsSpec.map {
      case Keep(name) => keepOutDims .find(_.name ==   name).get
      case c: Create  => alterOutDims.find(_.name == c.name).get
    }

    val outVar = writer.addVariable(null, inVar.getShortName, inVar.getDataType, outDims.asJava)

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

    (alterOutDimsD zip alterOutDimsV).foreach { case (dimData, outVarD) =>
      // val dt      = ma2.DataType.getType(dimData.getElementType)
      // val outVarD = writer.addVariable(null, dimName, dt, Seq(outDim).asJava)
      writer.write(outVarD, dimData)
    }

    // val transformShape: Vec[Int] = inDims.map(name => in.dimensionMap(name).size)
    val permutations: Array[Int] = inDims.map(name => alterInDims.indexWhere(_.name == name))(breakOut)

    val expectedRank  = alterOutDimsD.size
    val expectedSize  = alterOutDimsD.map(_.size).product
    val writeShape: Array[Int] = outDimsSpec.map {
      case Keep(_)    => 1
      case c: Create  => c.values.size.toInt
    } (breakOut)

    def iter(sec: VariableSection, origin: Vec[Int], rem: Vec[OutDim]): Unit = rem match {
      case head +: tail =>
        head match {
          case Keep(name) =>
            val dim = keepInDims.find(_.name == name).get
            for (i <- 0 until dim.size) {
              val sec1 = (sec in name).select(i)
              iter(sec1, origin :+ i, tail)
            }
          case c: Create =>
            iter(sec, origin :+ 0, tail)
        }

      case _ =>
        // `dataInF` has the original rank and dimensional order
        val dataInF = sec.readSafe()
        // `dataInR` removes the dimensions not included in `inDims`
        val dataInR = (dataInF /: allInDims.zipWithIndex) { case (a, (dim, idx)) =>
          if (inDims.contains(dim.name)) a else a.reduce(idx)
        }
        // `dataIn` transposes the dimensions to reflect the order in `inDims`
        val dataIn  = dataInR.permute(permutations)
        // println(s"Shape = ${dataIn.shape}")
        // assert(dataInF.getRank == inDims.size, s"Array has rank ${dataInF.getRank} while expecting ${inDims.size}")
        val originF = (origin zip outDimsSpec).collect {
          case (i, Keep(_)) => i
        }
        val dataOut = fun(originF, dataIn) // fun(dataIn)

        require(dataOut.getRank == expectedRank,
          s"Transformation expected to output rank $expectedRank (observed: ${dataOut.getRank})")
        require(dataOut.size == expectedSize,
          s"Transformation expected to produce $expectedSize values (observed: ${dataOut.size})")

        // println(s"Origin: $origin")
        val dataOutR = dataOut.reshapeNoCopy(writeShape)
        writer.write(outVar, origin.toArray, dataOutR)
    }

    iter(inVar.selectAll, Vec.empty, outDimsSpec)


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

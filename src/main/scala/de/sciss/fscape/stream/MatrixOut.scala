/*
 *  MatrixOut.scala
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

package de.sciss.fscape
package stream

import java.util

import akka.stream.stage.{GraphStageLogic, InHandler, OutHandler}
import akka.stream.{Attributes, FlowShape}
import at.iem.sysson.fscape.graph.Matrix
import de.sciss.file._
import de.sciss.fscape.stream.impl.{BlockingGraphStage, NodeImpl}
import de.sciss.lucre.matrix.impl.ReaderImpl
import ucar.ma2.DataType
import ucar.nc2.NetcdfFileWriter
import ucar.nc2.constants.CDM
import ucar.{ma2, nc2}

object MatrixOut {
  def apply(file: File, spec: Matrix.Spec.Value, in: OutD)(implicit b: Builder): OutL = {
    val source  = new Stage(file, spec)
    val stage   = b.add(source)
    b.connect(in, stage.in)
    stage.out
  }

  private final val name = "MatrixOut"

  private type Shape = FlowShape[BufD, BufL]

  private final class Stage(file: File, spec: Matrix.Spec.Value)(implicit ctrl: Control)
    extends BlockingGraphStage[Shape](s"$name($file)") {

    val shape = FlowShape(InD(s"$name.in"), OutL(s"$name.out"))

    def createLogic(attr: Attributes): NodeImpl[Shape] =
      new Logic(shape, file, spec)
  }

  private final class Logic(shape: Shape, protected val file: File, protected val spec: Matrix.Spec.Value)
                           (implicit ctrl: Control)
    extends NodeImpl(s"$name($file)", shape) with AbstractLogic

  trait AbstractLogic extends Node with InHandler with OutHandler { logic: GraphStageLogic =>
    // ---- abstract ----

    protected def file : File
    protected def spec : Matrix.Spec.Value
    protected def shape: Shape

    // ---- impl ----

    private[this] var framesRead  = 0L
    private[this] val numFrames   = spec.size
    private[this] var _isSuccess  = false
    private[this] val matShape    = spec.shape.toArray
    private[this] val rank        = matShape.length
    private[this] val arrShape    = new Array[Int](rank)
    private[this] val origin      = new Array[Int](rank)

    private[this] var writer: NetcdfFileWriter  = _
    private[this] var outVar: nc2.Variable      = _

    setHandler(shape.in , this)
    setHandler(shape.out, this)

    protected final def isSuccess     : Boolean  = _isSuccess

    final def onPull(): Unit = if (isAvailable(shape.in )) process()
    final def onPush(): Unit = if (isAvailable(shape.out)) process()

    override def preStart(): Unit = {
      writer      = NetcdfFileWriter.createNew(NetcdfFileWriter.Version.netcdf3, file.path, null)
      val varDims = new util.ArrayList[nc2.Dimension](rank)
      val dimsData = spec.dimensions.map { dimSpec =>
        val dim   = writer.addDimension(null, dimSpec.name, dimSpec.values.size)
        val dimL  = new util.ArrayList[nc2.Dimension](1)
        dimL.add(dim)
        varDims.add(dim)
        val dimVar = writer.addVariable(null, dimSpec.name, DataType.DOUBLE, dimL)
        if (!dimSpec.units.isEmpty)
          dimVar.addAttribute(new nc2.Attribute(CDM.UNITS, dimSpec.units))

        (dimVar, dimSpec.values)
      }

      outVar = writer.addVariable(null, spec.name, DataType.DOUBLE, varDims)
      if (!spec.units.isEmpty)
        outVar.addAttribute(new nc2.Attribute(CDM.UNITS, spec.units))

      // create the file; ends "define mode"
      writer.create()

      dimsData.foreach { case (dimVar, dimData) =>
        val arr = ma2.Array.factory(dimData.toArray)
        writer.write(dimVar, arr)
      }

      pull(shape.in)
    }

    override protected def stopped(): Unit = {
      if (!_isSuccess) writer.abort()
      super.stopped()
    }

    private def process(): Unit = {
      val bufOut  = control.borrowBufL()
      val bufIn   = grab(shape.in)
      tryPull(shape.in)
      val chunk = math.min(bufIn.size, numFrames - framesRead).toInt
      if (chunk > 0) {
        var _framesRead = framesRead
        var off         = 0

        ReaderImpl.partition(matShape, _framesRead, _framesRead + chunk) { ranges =>
          val _arrShape = arrShape
          val _origin   = origin
          var i = 0
          var len = 1
          while (i < _arrShape.length) {
            val r  = ranges(i)
            val sh = r.size
            _arrShape(i) = sh
            _origin  (i) = r.start
            len *= sh
            i   += 1
          }
          val arr   = ma2.Array.factory(DataType.DOUBLE, _arrShape)
          val arrD  = arr.getStorage.asInstanceOf[Array[Double]]
          System.arraycopy(bufIn.buf, off, arrD, 0, len)
          writer.write(outVar, _origin, arr)
          off += len
        }

        off = 0
        val out = bufOut.buf
        while (off < chunk) {
          _framesRead += 1
          out(off) = _framesRead
          off += 1
        }

        bufOut.size = chunk   // IntelliJ highlight bug
        push(shape.out, bufOut)

        framesRead = _framesRead
      }
      if (framesRead == numFrames) {
        logStream(s"completeStage() $this")
        writer.close()
        _isSuccess = true
        completeStage()
      }
    }
  }
}
/*
 *  BlobVoices.scala
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

import java.awt.Rectangle
import java.awt.geom.{Area, Path2D}

import akka.stream.stage.InHandler
import akka.stream.{Attributes, FanInShape10}
import de.sciss.fscape.stream.impl.{DemandAuxInHandler, DemandChunkImpl, DemandFilterLogic, DemandInOutImpl, DemandProcessInHandler, NodeImpl, Out1DoubleImpl, Out1LogicImpl, ProcessOutHandlerImpl, StageImpl}

import scala.annotation.{switch, tailrec}

object BlobVoices {
  def apply(in: OutD, width: OutI, height: OutI, minWidth: OutI, minHeight: OutI, voices: OutI,
            numBlobs: OutI, bounds: OutD, numVertices: OutI, vertices: OutD)(implicit b: Builder): OutD = {
    val stage0  = new Stage
    val stage   = b.add(stage0)
    b.connect(in         , stage.in0 )
    b.connect(width      , stage.in1 )
    b.connect(height     , stage.in2 )
    b.connect(minWidth   , stage.in3 )
    b.connect(minHeight  , stage.in4 )
    b.connect(voices     , stage.in5 )
    b.connect(numBlobs   , stage.in6 )
    b.connect(bounds     , stage.in7 )
    b.connect(numVertices, stage.in8 )
    b.connect(vertices   , stage.in9 )
    stage.out
  }

  private final val name = "BlobVoices"

  private type Shape = FanInShape10[BufD, BufI, BufI, BufI, BufI, BufI, BufI, BufD, BufI, BufD, BufD]

  private final class Stage(implicit ctrl: Control) extends StageImpl[Shape](name) {
    val shape = new FanInShape10(
      in0  = InD (s"$name.in"         ),
      in1  = InI (s"$name.width"      ),
      in2  = InI (s"$name.height"     ),
      in3  = InI (s"$name.minWidth"   ),
      in4  = InI (s"$name.minHeight"  ),
      in5  = InI (s"$name.voices"     ),
      in6  = InI (s"$name.numBlobs"   ),
      in7  = InD (s"$name.bounds"     ),
      in8  = InI (s"$name.numVertices"),
      in9  = InD (s"$name.vertices"   ),
      out  = OutD(s"$name.out"        )
    )

    def createLogic(attr: Attributes) = new Logic(shape)
  }

  private final class Blob {
    var xMin        = 0.0
    var xMax        = 0.0
    var yMin        = 0.0
    var yMax        = 0.0

    def width : Double = xMax - xMin
    def height: Double = yMax - yMin

    var numVertices = 0
    var vertexX: Array[Double] = _
    var vertexY: Array[Double] = _
  }

  private final class Logic(shape: Shape)(implicit ctrl: Control)
    extends NodeImpl(name, shape)
      with DemandFilterLogic[BufD, Shape]
      with DemandChunkImpl  [Shape]
      with Out1LogicImpl    [BufD, Shape]
      with DemandInOutImpl  [Shape]
      with Out1DoubleImpl   [Shape] {

    private[this] var winSizeIn         = 0
    private[this] var winSizeOut        = 0
    private[this] var winBufIn : Array[Double] = _
    private[this] var winBufOut: Array[Double] = _
    private[this] var width             = 0
    private[this] var height            = 0
    private[this] var minWidth          = 0
    private[this] var minHeight         = 0
    private[this] var voices            = 0

    private[this] var writeToWinOff     = 0
    private[this] var writeToWinRemain  = 0
    private[this] var readFromWinOff    = 0
    private[this] var readFromWinRemain = 0
    private[this] var isNextWindow      = true

    protected     var bufIn0 : BufD = _ // in
    private[this] var bufIn1 : BufI = _ // width
    private[this] var bufIn2 : BufI = _ // height
    private[this] var bufIn3 : BufI = _ // minWidth
    private[this] var bufIn4 : BufI = _ // minHeight
    private[this] var bufIn5 : BufI = _ // voices
    private[this] var bufIn6 : BufI = _ // numBlobs
    private[this] var bufIn7 : BufD = _ // bounds
    private[this] var bufIn8 : BufI = _ // numVertices
    private[this] var bufIn9 : BufD = _ // vertices
    protected     var bufOut0: BufD = _

    protected def in0: InD = shape.in0

    private[this] var _mainCanRead            = false
    private[this] var _auxCanRead             = false
    private[this] var _mainInValid            = false
    private[this] var _auxInValid             = false
    private[this] var _inValid                = false

    private[this] var _blobNumCanRead         = false
    private[this] var _blobBoundsCanRead      = false
    private[this] var _blobNumVerticesCanRead = false
    private[this] var _blobVerticesCanRead    = false

    private[this] var blobNumOff              = 0
    private[this] var blobNumRemain           = 0
    private[this] var blobBoundsOff           = 0
    private[this] var blobBoundsRemain        = 0
    private[this] var blobNumVerticesOff      = 0
    private[this] var blobNumVerticesRemain   = 0
    private[this] var blobVerticesOff         = 0
    private[this] var blobVerticesRemain      = 0

    private[this] var blobs: Array[Blob] = _
    private[this] var blobsBoundsRead         = 0
    private[this] var blobsNumVerticesRead    = 0
    private[this] var blobsVerticesMissing    = 0
    private[this] var blobsVerticesBlobIdx    = 0
    private[this] var blobsVerticesVertexIdx  = 0

    private object BlobNumInHandler extends InHandler {
      def onPush(): Unit =
        if (canReadBlobNum) {
          readBlobNumIn()
          process()
        }

      override def onUpstreamFinish(): Unit = {
        if (canReadBlobNum) readBlobNumIn()
        process()
      }
    }

    private object BlobBoundsInHandler extends InHandler {
      def onPush(): Unit =
        if (canReadBlobBounds) {
          readBlobBoundsIn()
          process()
        }

      override def onUpstreamFinish(): Unit = {
        if (canReadBlobBounds) readBlobBoundsIn()
        process()
      }
    }

    private object BlobNumVerticesInHandler extends InHandler {
      def onPush(): Unit =
        if (canReadBlobNumVertices) {
          readBlobNumVerticesIn()
          process()
        }

      override def onUpstreamFinish(): Unit = {
        if (canReadBlobNumVertices) readBlobNumVerticesIn()
        process()
      }
    }

    private object BlobVerticesInHandler extends InHandler {
      def onPush(): Unit =
        if (canReadBlobVertices) {
          readBlobVerticesIn()
          process()
        }

      override def onUpstreamFinish(): Unit = {
        if (canReadBlobVertices) readBlobVerticesIn()
        process()
      }
    }

    new DemandProcessInHandler(shape.in0 , this)
    new DemandAuxInHandler    (shape.in1 , this)
    new DemandAuxInHandler    (shape.in2 , this)
    new DemandAuxInHandler    (shape.in3 , this)
    new DemandAuxInHandler    (shape.in4 , this)
    new DemandAuxInHandler    (shape.in5 , this)
    setInHandler(shape.in6, BlobNumInHandler        )
    setInHandler(shape.in7, BlobBoundsInHandler     )
    setInHandler(shape.in8, BlobNumVerticesInHandler)
    setInHandler(shape.in9, BlobVerticesInHandler   )
    new ProcessOutHandlerImpl (shape.out , this)

    @inline private[this] def canWriteToWindow        = readFromWinRemain     == 0 && inValid && !_statePrepareProcess
    @inline private[this] def canReadBlobNum          = blobNumRemain         == 0 && isAvailable(shape.in6)
    @inline private[this] def canReadBlobBounds       = blobBoundsRemain      == 0 && isAvailable(shape.in7)
    @inline private[this] def canReadBlobNumVertices  = blobNumVerticesRemain == 0 && isAvailable(shape.in8)
    @inline private[this] def canReadBlobVertices     = blobVerticesRemain    == 0 && isAvailable(shape.in9)

    @inline private[this] def blobNumEnded            = !isAvailable(shape.in6) && isClosed(shape.in6)
    @inline private[this] def blobBoundsEnded         = !isAvailable(shape.in7) && isClosed(shape.in7)
    @inline private[this] def blobNumVerticesEnded    = !isAvailable(shape.in8) && isClosed(shape.in8)
    @inline private[this] def blobVerticesEnded       = !isAvailable(shape.in9) && isClosed(shape.in9)

    protected def out0: OutD = shape.out

    def mainCanRead : Boolean = _mainCanRead
    def auxCanRead  : Boolean = _auxCanRead
    def mainInValid : Boolean = _mainInValid
    def auxInValid  : Boolean = _auxInValid
    def inValid     : Boolean = _inValid

    override def preStart(): Unit = {
      val sh = shape
      pull(sh.in0)
      pull(sh.in1)
      pull(sh.in2)
      pull(sh.in3)
      pull(sh.in4)
      pull(sh.in5)
      pull(sh.in6)
      pull(sh.in7)
      pull(sh.in8)
      pull(sh.in9)
    }

    override protected def stopped(): Unit = {
      freeInputBuffers()
      freeOutputBuffers()
      winBufIn    = null
      winBufOut   = null
      blobs       = null
    }

    protected def readMainIns(): Int = {
      freeMainInBuffers()
      val sh        = shape
      bufIn0        = grab(sh.in0)
      bufIn0.assertAllocated()
      tryPull(sh.in0)

      if (!_mainInValid) {
        _mainInValid= true
        _inValid    = _auxInValid
      }

      _mainCanRead = false
      bufIn0.size
    }

    protected def readAuxIns(): Int = {
      freeAuxInBuffers()
      val sh    = shape
      var sz    = 0

      if (isAvailable(sh.in1)) {  // width
        bufIn1  = grab(sh.in1)
        sz      = bufIn1.size
        tryPull(sh.in1)
      }
      if (isAvailable(sh.in2)) {  // height
        bufIn2  = grab(sh.in2)
        sz      = math.max(sz, bufIn2.size)
        tryPull(sh.in2)
      }
      if (isAvailable(sh.in3)) {  // minWidth
        bufIn3  = grab(sh.in3)
        sz      = math.max(sz, bufIn3.size)
        tryPull(sh.in3)
      }
      if (isAvailable(sh.in4)) {  // minHeight
        bufIn4  = grab(sh.in4)
        sz      = math.max(sz, bufIn4.size)
        tryPull(sh.in4)
      }
      if (isAvailable(sh.in5)) {  // voices
        bufIn5  = grab(sh.in5)
        sz      = math.max(sz, bufIn5.size)
        tryPull(sh.in5)
      }

      if (!_auxInValid) {
        _auxInValid = true
        _inValid    = _mainInValid
      }

      _auxCanRead = false
      sz
    }

    private def readBlobNumIn(): Unit = {
      require(blobNumRemain == 0)
      freeBlobNumInBuffer()
      bufIn6                = grab(shape.in6)
      blobNumOff            = 0
      blobNumRemain         = bufIn6.size
      tryPull(shape.in6)
    }

    private def readBlobBoundsIn(): Unit = {
      require(blobBoundsRemain == 0)
      freeBlobBoundsInBuffer()
      bufIn7                = grab(shape.in7)
      blobBoundsOff         = 0
      blobBoundsRemain      = bufIn7.size
      tryPull(shape.in7)
    }

    private def readBlobNumVerticesIn(): Unit = {
      require(blobNumVerticesRemain == 0)
      freeBlobNumVerticesInBuffer()
      bufIn8                = grab(shape.in8)
      blobNumVerticesOff    = 0
      blobNumVerticesRemain = bufIn8.size
      tryPull(shape.in8)
    }

    private def readBlobVerticesIn(): Unit = {
      require(blobVerticesRemain == 0)
      freeBlobVerticesInBuffer()
      bufIn9                = grab(shape.in9)
      blobVerticesOff       = 0
      blobVerticesRemain    = bufIn9.size
      tryPull(shape.in9)
    }

    private def freeInputBuffers(): Unit = {
      freeMainInBuffers()
      freeAuxInBuffers()
      freeBlobNumInBuffer()
      freeBlobBoundsInBuffer()
      freeBlobNumVerticesInBuffer()
      freeBlobVerticesInBuffer()
    }

    private def freeMainInBuffers(): Unit =
      if (bufIn0 != null) {
        bufIn0.release()
        bufIn0 = null
      }

    private def freeAuxInBuffers(): Unit = {
      if (bufIn1 != null) {
        bufIn1.release()
        bufIn1 = null
      }
      if (bufIn2 != null) {
        bufIn2.release()
        bufIn2 = null
      }
      if (bufIn3 != null) {
        bufIn3.release()
        bufIn3 = null
      }
      if (bufIn4 != null) {
        bufIn4.release()
        bufIn4 = null
      }
      if (bufIn5 != null) {
        bufIn5.release()
        bufIn5 = null
      }
    }

    private def freeBlobNumInBuffer(): Unit =
      if (bufIn6 != null) {
        bufIn6.release()
        bufIn6 = null
      }

    private def freeBlobBoundsInBuffer(): Unit =
      if (bufIn7 != null) {
        bufIn7.release()
        bufIn7 = null
      }

    private def freeBlobNumVerticesInBuffer(): Unit =
      if (bufIn8 != null) {
        bufIn8.release()
        bufIn8 = null
      }

    private def freeBlobVerticesInBuffer(): Unit =
      if (bufIn9 != null) {
        bufIn9.release()
        bufIn9 = null
      }

    protected def freeOutputBuffers(): Unit =
      if (bufOut0 != null) {
        bufOut0.release()
        bufOut0 = null
      }

    def updateMainCanRead(): Unit =
      _mainCanRead = isAvailable(in0)

    def updateAuxCanRead(): Unit = {
      val sh = shape
      _auxCanRead =
        ((isClosed(sh.in1) && _auxInValid) || isAvailable(sh.in1)) &&
        ((isClosed(sh.in2) && _auxInValid) || isAvailable(sh.in2)) &&
        ((isClosed(sh.in3) && _auxInValid) || isAvailable(sh.in3)) &&
        ((isClosed(sh.in4) && _auxInValid) || isAvailable(sh.in4)) &&
        ((isClosed(sh.in5) && _auxInValid) || isAvailable(sh.in5))
    }

    @inline private def updateBlobNumCanRead        (): Unit = _blobNumCanRead          = isAvailable(shape.in6)
    @inline private def updateBlobBoundsCanRead     (): Unit = _blobBoundsCanRead       = isAvailable(shape.in7)
    @inline private def updateBlobNumVerticesCanRead(): Unit = _blobNumVerticesCanRead  = isAvailable(shape.in8)
    @inline private def updateBlobVerticesCanRead   (): Unit = _blobVerticesCanRead     = isAvailable(shape.in9)

    private[this] var _statePrepareProcess      = false
    private[this] var _stateReadBlobNum         = false
    private[this] var _stateReadBlobBounds      = false
    private[this] var _stateReadBlobNumVertices = false
    private[this] var _stateReadBlobVertices    = false
    private[this] var _stateProcessBlobs        = false

    private[this] var _stateComplete            = false

    private[this] var numBlobs                  = 0

    protected def processChunk(): Boolean = {
      var stateChange = false

      if (canWriteToWindow) {
        val flushIn0 = inputsEnded // inRemain == 0 && shouldComplete()
        if (isNextWindow && !flushIn0) {
          writeToWinRemain  = startNextWindow()
          isNextWindow      = false
          stateChange       = true
          // logStream(s"startNextWindow(); writeToWinRemain = $writeToWinRemain")
        }

        val chunk     = math.min(writeToWinRemain, mainInRemain) // .toInt
        val flushIn   = flushIn0 && writeToWinOff > 0
        if (chunk > 0 || flushIn) {
          // logStream(s"writeToWindow(); inOff = $inOff, writeToWinOff = $writeToWinOff, chunk = $chunk")
          if (chunk > 0) {
            copyInputToWindow(writeToWinOff = writeToWinOff, chunk = chunk)
            mainInOff        += chunk
            mainInRemain     -= chunk
            writeToWinOff    += chunk
            writeToWinRemain -= chunk
            stateChange       = true
          }

          if (writeToWinRemain == 0 || flushIn) {
            _statePrepareProcess  = true
            _stateReadBlobNum     = true
            stateChange           = true
            // logStream(s"processWindow(); readFromWinRemain = $readFromWinRemain")
          }
        }
      }

      if (_stateReadBlobNum) {
        if (canReadBlobNum) readBlobNumIn()
        if (blobNumRemain > 0) {
          numBlobs            = bufIn6.buf(blobNumOff)
          if (blobs == null || blobs.length < numBlobs) {
            blobs = Array.fill[Blob](numBlobs)(new Blob)
          }
          blobNumRemain            -= 1
          blobNumOff               += 1
          blobsBoundsRead           = 0
          blobsVerticesMissing      = 0
          blobsVerticesBlobIdx      = 0
          blobsVerticesVertexIdx    = 0
          _stateReadBlobNum         = false
          _stateReadBlobBounds      = true
          _stateReadBlobNumVertices = true
          _stateReadBlobVertices    = true
          stateChange               = true
        } else if (blobNumEnded) {
          _stateComplete            = true
          return stateChange
        }
      }

      if (_stateReadBlobBounds) {
        if (canReadBlobBounds) readBlobBoundsIn()
        val chunk = math.min(blobBoundsRemain, numBlobs * 4 - blobsBoundsRead)
        if (chunk > 0) {
          var _boundsOff  = blobBoundsOff
          var _boundsRead = blobsBoundsRead
          val _buf        = bufIn7.buf
          val stop        = _boundsOff + chunk
          while (_boundsOff < stop) {
            val blobIdx = _boundsRead / 4
            val blob    = blobs(blobIdx)
            val coord   = _buf(_boundsOff)
            (_boundsRead % 4: @switch) match {
              case 0 => blob.xMin = coord
              case 1 => blob.xMax = coord
              case 2 => blob.yMin = coord
              case 3 => blob.yMax = coord
            }
            _boundsOff  += 1
            _boundsRead += 1
          }
          blobBoundsOff     = _boundsOff
          blobBoundsRemain -= chunk
          blobsBoundsRead   = _boundsRead
          stateChange       = true
        }
        if (blobsBoundsRead == numBlobs * 4) {
          _stateReadBlobBounds      = false
          _stateProcessBlobs        = !(_stateReadBlobNumVertices || _stateReadBlobVertices)
          stateChange               = true

        } else if (blobBoundsRemain == 0 && blobBoundsEnded) {
          _stateComplete            = true
          return stateChange
        }
      }

      if (_stateReadBlobNumVertices) {
        if (canReadBlobNumVertices) readBlobNumVerticesIn()
        val chunk = math.min(blobNumVerticesRemain, numBlobs - blobsNumVerticesRead)
        if (chunk > 0) {
          var _numVerticesOff   = blobNumVerticesOff
          var _numVerticesRead  = blobsNumVerticesRead
          val _buf              = bufIn8.buf
          val stop              = _numVerticesOff + chunk
          while (_numVerticesOff < stop) {
            val blobIdx       = _numVerticesRead
            val blob          = blobs(_numVerticesRead)
            val num           = _buf(_numVerticesOff)
            blob.numVertices  = num
            blob.vertexX      = new Array[Double](num)
            blob.vertexY      = new Array[Double](num)
            blobsVerticesMissing += num * 2
            _numVerticesOff  += 1
            _numVerticesRead += 1
          }
          blobNumVerticesOff    = _numVerticesOff
          blobNumVerticesRemain -= chunk
          blobsNumVerticesRead  = _numVerticesRead
          stateChange           = true
        }
        if (blobsNumVerticesRead == numBlobs) {
          _stateReadBlobNumVertices = false
          _stateProcessBlobs        = !(_stateReadBlobBounds || _stateReadBlobVertices)
          stateChange               = true

        } else if (blobNumVerticesRemain == 0 && blobNumVerticesEnded) {
          _stateComplete            = true
          return stateChange
        }
      }

      if (_stateReadBlobVertices) {
        if (canReadBlobVertices) readBlobVerticesIn()
        val chunk = math.min(blobVerticesRemain, blobsVerticesMissing)
        if (chunk > 0) {
          var _verticesOff    = blobVerticesOff
          val _buf            = bufIn9.buf
          var _blobIdx        = blobsVerticesBlobIdx
          var _vIdx           = blobsVerticesVertexIdx
          val stop            = _verticesOff + chunk
          while (_verticesOff < stop) {
            val blob    = blobs(_blobIdx)
            val num     = blob.numVertices
            val chunk2  = math.min(num * 2 - _vIdx, stop - _verticesOff)
            if (chunk2 > 0) {
              val stop2 = _verticesOff + chunk2
              while (_verticesOff < stop2) {
                val coord     = _buf(_verticesOff)
                val table     = if (_vIdx % 2 == 0) blob.vertexX else blob.vertexY
                table(_vIdx / 2) = coord
                _verticesOff += 1
                _vIdx        += 1
              }
            } else {
              _blobIdx += 1
              _vIdx     = 0
            }
          }
          blobsVerticesBlobIdx    = _blobIdx
          blobsVerticesVertexIdx  = _vIdx
          blobsVerticesMissing   -= chunk
          blobVerticesOff         = _verticesOff
          blobVerticesRemain     -= chunk
          stateChange             = true
        }
        if (blobsVerticesMissing == 0 && !_stateReadBlobNumVertices) {
          _stateReadBlobVertices    = false
          _stateProcessBlobs        = !_stateReadBlobBounds
          stateChange               = true

        } else if (blobVerticesRemain == 0 && blobVerticesEnded) {
          _stateComplete            = true
          return stateChange
        }
      }

      if (_stateProcessBlobs) {
        readFromWinRemain     = processWindow(writeToWinOff = writeToWinOff) // , flush = flushIn)
        writeToWinOff         = 0
        readFromWinOff        = 0
        isNextWindow          = true
        auxInOff             += 1
        auxInRemain          -= 1
        _stateProcessBlobs    = false
        _statePrepareProcess  = false
        stateChange           = true
      }

      if (readFromWinRemain > 0) {
        val chunk = math.min(readFromWinRemain, outRemain) // .toInt
        if (chunk > 0) {
          // logStream(s"readFromWindow(); readFromWinOff = $readFromWinOff, outOff = $outOff, chunk = $chunk")
          copyWindowToOutput(readFromWinOff = readFromWinOff, outOff = outOff, chunk = chunk)
          readFromWinOff    += chunk
          readFromWinRemain -= chunk
          outOff            += chunk
          outRemain         -= chunk
          stateChange        = true
        }
      }

      stateChange
    }

    protected def shouldComplete(): Boolean =
      _stateComplete || (inputsEnded && writeToWinOff == 0 && readFromWinRemain == 0)

    private def startNextWindow(): Int = {
      val oldWinSzIn  = winSizeIn
      val oldWinSzOut = winSizeOut
      val inOff       = auxInOff
      if (bufIn1 != null && inOff < bufIn1.size) {
        width = math.max(1, bufIn1.buf(inOff))
      }
      if (bufIn2 != null && inOff < bufIn2.size) {
        height = math.max(1, bufIn2.buf(inOff))
      }
      if (bufIn3 != null && inOff < bufIn3.size) {
        minWidth = bufIn3.buf(inOff)
      }
      if (bufIn4 != null && inOff < bufIn4.size) {
        minHeight = bufIn4.buf(inOff)
      }
      if (bufIn5 != null && inOff < bufIn5.size) {
        voices = math.max(1, bufIn5.buf(inOff))
      }
      winSizeIn = width * height
      if (winSizeIn != oldWinSzIn) {
        winBufIn = new Array[Double](winSizeIn)
      }
      val blobDimSz = BlobVoice.totalNumField * voices
      winSizeOut = blobDimSz * width
      if (winSizeOut != oldWinSzOut) {
        winBufOut = new Array[Double](winSizeOut)
      }
      winSizeIn
    }

    private def copyInputToWindow(writeToWinOff: Int, chunk: Int): Unit =
      Util.copy(bufIn0.buf, mainInOff, winBufIn, writeToWinOff, chunk)

    private def copyWindowToOutput(readFromWinOff: Int, outOff: Int, chunk: Int): Unit =
      Util.copy(winBufOut, readFromWinOff, bufOut0.buf, outOff, chunk)

    // ---- the fun bit ----
    // this is mostly the translation from sysson-experiments/AnomaliesBlobs.scala

    private def processWindow(writeToWinOff: Int): Int = {
      // if (writeToWinOff == 0) return writeToWinOff

      val _blobs      = blobs
      val _width      = width
      val _height     = height
      val _minWidth   = minWidth
      val _minHeight  = minHeight
      val _numBlobs   = numBlobs
      val _bufIn      = winBufIn
      val _thresh     = 1.0   // XXX TODO --- user customizable

      val blobsAllB = Vector.newBuilder[BlobVoice]
      blobsAllB.sizeHint(_numBlobs)

      val path  = new Path2D.Double
      val rect  = new Rectangle
      val area  = new Area

      var blobIdx = 0
      while (blobIdx < _numBlobs) {
        val blob  = _blobs(blobIdx)
        val ok    = blob.width >= _minWidth && blob.height >= _minHeight
        if (ok) {
          path.reset()
          var vIdx = 0
          while (vIdx < blob.numVertices) {
            val x = blob.vertexX(vIdx)
            val y = blob.vertexY(vIdx)
            if (vIdx == 0) {
              path.moveTo(x, y)
            } else {
              path.lineTo(x, y)
            }
            vIdx += 1
          }
          path.closePath()

          area.reset()
          val br          = path.getBounds
          val blobLeft    = math.max(0, br.x /* - 1 */)
          val blobWidth   = math.min(_width - blobLeft, br.width)
          val blobRight   = blobLeft + blobWidth
          val blobTop     = math.max(0, br.y /* - 1 */)
          val blobHeight  = math.min(_height  - blobTop , br.height)
          val blobBottom  = blobTop + blobHeight

          val slices      = new Array[BlobSlice](blobWidth)

          var x           = blobLeft
          var sliceIdx    = 0
          while (x < blobRight) {
            rect.x             = x /* + 1 */
            rect.y             = 0
            rect.width         = 1
            rect.height        = height
            val a           = new Area(path)
            a.intersect(new Area(rect))
            val b           = a.getBounds2D
            area.add(new Area(b))
            val boxTop      = math.max(blobTop   , math.floor(b.getMinY).toInt)
            val boxBottom   = math.min(blobBottom, math.ceil (b.getMaxY).toInt)
            val boxHeight   = boxBottom - boxTop
            var y           = boxTop
            var sliceSum    = 0.0
            var sliceCenter = 0.0
            var sliceCnt    = 0
            while (y < boxBottom) {
//              val value = _bufIn(x * _height + y)
              val value = _bufIn(x + _width * y)
              if (value > _thresh) {
                sliceSum    += value
                sliceCenter += value * y
                sliceCnt    += 1
              }
              y += 1
            }
            import de.sciss.numbers.Implicits._
            val sliceMean = sliceSum / boxHeight
            sliceCenter   = (sliceCenter / sliceSum).clip(boxTop, boxBottom - 1)

            y = boxTop
            var sliceStdDev = 0.0
            while (y < boxBottom) {
//              val value  = _bufIn(x * _height + y)
              val value = _bufIn(x + _width * y)
              if (value > _thresh) {
              val d = value - sliceMean
                sliceStdDev += d * d
              }
              y += 1
            }
            if (sliceCnt > 0) sliceStdDev = math.sqrt(sliceStdDev / (sliceCnt - 1))

            val slice = BlobSlice(
              boxTop        = boxTop,
              boxHeight     = boxHeight,
              sliceMean     = sliceMean,
              sliceStdDev   = sliceStdDev,
              sliceCenter   = sliceCenter
            )

            slices(sliceIdx) = slice
            x  += 1
            sliceIdx += 1
          }
          // bloody floating point ops and rounding can lead to difference here
          //        val ri = out.getBounds
          //        assert(ri == br, s"ri = $ri; br = $br")

          val bv = BlobVoice(
            id          = -1,
            blobLeft    = blobLeft,
            blobTop     = blobTop,
            blobWidth   = blobWidth,
            blobHeight  = blobHeight,
            slices      = slices
          )
          blobsAllB += bv
        }
        blobIdx += 1
      }

      val blobsAll: Vector[BlobVoice] = blobsAllB.result()
      val _voices = voices

      // call with shapes sorted by size in ascending order!
      @tailrec def filterOverlaps(rem: Vector[BlobVoice], out: Vector[BlobVoice], id: Int): Vector[BlobVoice] =
      rem match {
        case head +: tail =>
          val numOverlap = tail.count(_.overlaps(head))
          val idNext  = if (numOverlap > _voices) id  else id + 1
          val outNext = if (numOverlap > _voices) out else out :+ head.copy(id = id)
          filterOverlaps(rem = tail, out = outNext, id = idNext)

        case _ => out
      }

      val blobFlt = filterOverlaps(blobsAll.sortBy(_.blobSize), out = Vector.empty, id = 1)
        .sortBy(b => (b.blobLeft, b.blobTop))

      val blobDimSz = BlobVoice.totalNumField * _voices
      val _bufOut   = winBufOut // Array.ofDim[Double](_width, blobDimSz)

      val idIndices = 0 until blobDimSz by BlobVoice.totalNumField

      @tailrec def mkArray(x: Int, activeBefore: Vector[BlobVoice], rem: Vector[BlobVoice]): Unit =
        if (x < _width) {
          val active1   = activeBefore .filterNot(_.blobRight == x)
          val (activeAdd, remRem) = rem.partition(_.blobLeft  == x)
          val activeNow = active1 ++ activeAdd
          val (activeOld, activeNew) = activeNow.partition(activeBefore.contains)
          if (activeOld.nonEmpty) {
            val xM = x - 1
            activeOld.foreach { blob =>
              val sliceIdx  = x - blob.blobLeft
              val outY      = idIndices.find { y =>
                _bufOut(xM + y * _width) == blob.id
              } .get  // same slot as before
              blob.fillSlice(sliceIdx = sliceIdx, out = _bufOut, off = x + outY * _width, scan = _width)
            }
          }
          if (activeNew.nonEmpty) {
            activeNew.foreach { blob =>
              val sliceIdx  = x - blob.blobLeft
              val outY      = idIndices.find { y =>
                _bufOut(x + y * _width) == 0
              } .get  // empty slot
              blob.fillSlice(sliceIdx = sliceIdx, out = _bufOut, off = x + outY * _width, scan = _width)
            }
          }
          mkArray(x = x + 1, activeBefore = activeNow, rem = remRem)
        } else {
          assert(rem.isEmpty)
        }

      Util.clear(_bufOut, 0, winSizeOut)
      mkArray(0, Vector.empty, blobFlt)

      winSizeOut
    }
  }

  private object BlobSlice {
    final val numFields: Int = BlobSlice(0, 0, 0, 0, 0).productArity
  }

  private final case class BlobSlice(boxTop: Int, boxHeight: Int, sliceMean: Double, sliceStdDev: Double,
                                     sliceCenter: Double) {

    def boxBottom: Int = boxTop + boxHeight

//    def toArray: Array[Float] =
//      Array[Float](boxTop, boxHeight, sliceMean.toFloat, sliceStdDev.toFloat, sliceCenter.toFloat)

    def fill(out: Array[Double], off: Int, scan: Int): Unit = {
      var _off = off
      out(_off) = boxTop       ; _off += scan
      out(_off) = boxHeight    ; _off += scan
      out(_off) = sliceMean    ; _off += scan
      out(_off) = sliceStdDev  ; _off += scan
      out(_off) = sliceCenter  ; _off += scan
    }
  }

  private object BlobVoice {
    final val numBaseFields: Int = BlobVoice(0, 0, 0, 0, 0, Array.empty).productArity - 1
    final val totalNumField: Int = numBaseFields + BlobSlice.numFields
  }
  /* @param id           unique blob identifier, positive. if zero, blob data is invalid
   * @param blobLeft     blob beginning in time frames ("horizontally")
   * @param blobTop      blob beginning within time slice (vertically)
   * @param blobWidth    blob duration in time frames
   * @param blobHeight   blob extent within time slice
   * @param slices       blob form
   */
  private final case class BlobVoice(id: Int, blobLeft: Int, blobTop: Int, blobWidth: Int, blobHeight: Int,
                                     slices: Array[BlobSlice]) {

    def blobRight   : Int = blobLeft  + blobWidth
    def blobBottom  : Int = blobTop   + blobHeight
    def blobSize    : Int = blobWidth * blobHeight

    def overlaps(that: BlobVoice): Boolean =
      this.blobLeft < that.blobRight  && this.blobRight  > that.blobLeft &&
      this.blobTop  < that.blobBottom && this.blobBottom > that.blobTop && {
        val left  = math.max(this.blobLeft , that.blobLeft )
        val right = math.min(this.blobRight, that.blobRight)
        var idx   = left
        var found = false
        while (idx < right) {
          val thisSlice = this.slices(idx - this.blobLeft)
          val thatSlice = that.slices(idx - that.blobLeft)
          found = thisSlice.boxTop < thatSlice.boxBottom && thisSlice.boxBottom > thatSlice.boxTop
          idx += 1
        }
        found
      }

//    def toArray(sliceIdx: Int): Array[Float] =
//      Array[Float](id, blobLeft, blobTop, blobWidth, blobHeight) ++ slices(sliceIdx).toArray

    def fillSlice(sliceIdx: Int, out: Array[Double], off: Int, scan: Int): Unit = {
      var _off = off
      out(_off) = id          ; _off += scan
      out(_off) = blobLeft    ; _off += scan
      out(_off) = blobTop     ; _off += scan
      out(_off) = blobWidth   ; _off += scan
      out(_off) = blobHeight  ; _off += scan
      val slice = slices(sliceIdx)
      slice.fill(out, off = _off, scan = scan)
    }
  }
}
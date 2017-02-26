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

import akka.stream.stage.InHandler
import akka.stream.{Attributes, FanInShape11}
import de.sciss.fscape.stream.impl.{DemandAuxInHandler, DemandChunkImpl, DemandFilterLogic, DemandInOutImpl, DemandProcessInHandler, NodeImpl, Out1DoubleImpl, Out1LogicImpl, ProcessOutHandlerImpl, StageImpl}

object BlobVoices {
  def apply(in: OutD, width: OutI, height: OutI, minWidth: OutI, minHeight: OutI, voices: OutI,
            numBlobs: OutI, xMin: OutI, xMax: OutI, yMin: OutI, yMax: OutI)(implicit b: Builder): OutD = {
    val stage0  = new Stage
    val stage   = b.add(stage0)
    b.connect(in       , stage.in0 )
    b.connect(width    , stage.in1 )
    b.connect(height   , stage.in2 )
    b.connect(minWidth , stage.in3 )
    b.connect(minHeight, stage.in4 )
    b.connect(voices   , stage.in5 )
    b.connect(numBlobs , stage.in6 )
    b.connect(xMin     , stage.in7 )
    b.connect(xMax     , stage.in8 )
    b.connect(yMin     , stage.in9 )
    b.connect(yMax     , stage.in10)
    stage.out
  }

  private final val name = "BlobVoices"

  private type Shape = FanInShape11[BufD, BufI, BufI, BufI, BufI, BufI, BufI, BufI, BufI, BufI, BufI, BufD]

  private final class Stage(implicit ctrl: Control) extends StageImpl[Shape](name) {
    val shape = new FanInShape11(
      in0  = InD (s"$name.in"       ),
      in1  = InI (s"$name.width"    ),
      in2  = InI (s"$name.height"   ),
      in3  = InI (s"$name.minWidth" ),
      in4  = InI (s"$name.minHeight"),
      in5  = InI (s"$name.voices"   ),
      in6  = InI (s"$name.numBlobs" ),
      in7  = InI (s"$name.xMin"     ),
      in8  = InI (s"$name.xMax"     ),
      in9  = InI (s"$name.yMin"     ),
      in10 = InI (s"$name.yMax"     ),
      out  = OutD(s"$name.out"      )
    )

    def createLogic(attr: Attributes) = new Logic(shape)
  }

  private final case class BlobData(xMin: Int, xMax: Int, yMin: Int, yMax: Int)

  private final class Logic(shape: Shape)(implicit ctrl: Control)
    extends NodeImpl(name, shape)
      with DemandFilterLogic[BufD, Shape]
      with DemandChunkImpl  [Shape]
      with Out1LogicImpl    [BufD, Shape]
      with DemandInOutImpl  [Shape]
      with Out1DoubleImpl   [Shape] {

    private[this] var winSize           = 0
    private[this] var winBuf: Array[Double] = _
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
    private[this] var bufIn7 : BufI = _ // xMin
    private[this] var bufIn8 : BufI = _ // xMax
    private[this] var bufIn9 : BufI = _ // yMin
    private[this] var bufIn10: BufI = _ // yMax
    protected     var bufOut0: BufD = _

    protected def in0: InD = shape.in0

    private[this] var _mainCanRead      = false
    private[this] var _auxCanRead       = false
    private[this] var _mainInValid      = false
    private[this] var _auxInValid       = false
    private[this] var _inValid          = false

    private[this] var _blobNumCanRead   = false
    private[this] var _blobDataCanRead  = false

    private[this] var blobNumOff        = 0
    private[this] var blobNumRemain     = 0
    private[this] var blobDataOff       = 0
    private[this] var blobDataRemain    = 0

    private[this] var blobData: Array[BlobData] = _
    private[this] var blobsRead         = 0

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

    private final class BlobDataInHandler(in: InI) extends InHandler {
      def onPush(): Unit =
        if (canReadBlobData) {
          readBlobDataIn()
          process()
        }

      override def onUpstreamFinish(): Unit = {
        if (canReadBlobData) readBlobDataIn()
        process()
      }

      setInHandler(in, this)
    }

    new DemandProcessInHandler(shape.in0 , this)
    new DemandAuxInHandler    (shape.in1 , this)
    new DemandAuxInHandler    (shape.in2 , this)
    new DemandAuxInHandler    (shape.in3 , this)
    new DemandAuxInHandler    (shape.in4 , this)
    new DemandAuxInHandler    (shape.in5 , this)
    setInHandler(shape.in6, BlobNumInHandler)
    new BlobDataInHandler     (shape.in7 )
    new BlobDataInHandler     (shape.in8 )
    new BlobDataInHandler     (shape.in9 )
    new BlobDataInHandler     (shape.in10)
    new ProcessOutHandlerImpl (shape.out , this)

    @inline
    private[this] def canWriteToWindow = readFromWinRemain == 0 && inValid

    @inline
    private[this] def canReadBlobNum = blobNumRemain == 0 && isAvailable(shape.in6)

    @inline
    private[this] def blobNumEnded = !isAvailable(shape.in6) && isClosed(shape.in6)

    @inline
    private[this] def blobDataEnded =
      (!isAvailable(shape.in7 ) && isClosed(shape.in7 )) ||
      (!isAvailable(shape.in8 ) && isClosed(shape.in8 )) ||
      (!isAvailable(shape.in9 ) && isClosed(shape.in9 )) ||
      (!isAvailable(shape.in10) && isClosed(shape.in10))

    @inline
    private[this] def canReadBlobData = blobDataRemain == 0 &&
      isAvailable(shape.in7) &&
      isAvailable(shape.in8) &&
      isAvailable(shape.in9) &&
      isAvailable(shape.in10)

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
      pull(sh.in10)
    }

    override protected def stopped(): Unit = {
      freeInputBuffers()
      freeOutputBuffers()
      winBuf    = null
      blobData  = null
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
      bufIn6        = grab(shape.in6)
      blobNumOff    = 0
      blobNumRemain = bufIn6.size
      tryPull(shape.in6)
    }

    private def readBlobDataIn(): Unit = {
      require(blobDataRemain == 0)
      freeBlobDataInBuffers()
      var sz = Int.MaxValue
      val sh = shape

      bufIn7  = grab(sh.in7)
      sz      = math.min(sz, bufIn1.size)
      tryPull(sh.in7)

      bufIn8  = grab(sh.in8)
      sz      = math.min(sz, bufIn2.size)
      tryPull(sh.in2)

      bufIn9  = grab(sh.in9)
      sz      = math.min(sz, bufIn9.size)
      tryPull(sh.in9)

      bufIn9  = grab(sh.in9)
      sz      = math.min(sz, bufIn9.size)
      tryPull(sh.in9)

      blobDataOff     = 0
      blobDataRemain  = sz
    }

    private def freeInputBuffers(): Unit = {
      freeMainInBuffers()
      freeAuxInBuffers()
      freeBlobNumInBuffer()
      freeBlobDataInBuffers()
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

    private def freeBlobDataInBuffers(): Unit = {
      if (bufIn7 != null) {
        bufIn7.release()
        bufIn7 = null
      }
      if (bufIn8 != null) {
        bufIn8.release()
        bufIn8 = null
      }
      if (bufIn9 != null) {
        bufIn9.release()
        bufIn9 = null
      }
      if (bufIn10 != null) {
        bufIn10.release()
        bufIn10 = null
      }
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

    private def updateBlobNumCanRead(): Unit =
      _blobNumCanRead = isAvailable(shape.in6)

    private def updateBlobDataCanRead(): Unit =
      _blobDataCanRead =
        isAvailable(shape.in7) &&
        isAvailable(shape.in8) &&
        isAvailable(shape.in9) &&
        isAvailable(shape.in10)

    private[this] var _stateReadBlobNum   = false
    private[this] var _stateReadBlobData  = false
    private[this] var _stateProcessBlobs  = false

    private[this] var _stateComplete      = false

    private[this] var numBlobs            = 0

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
            _stateReadBlobNum = true
            stateChange       = true
            // logStream(s"processWindow(); readFromWinRemain = $readFromWinRemain")
          }
        }
      }

      if (_stateReadBlobNum) {
        if (canReadBlobNum) readBlobNumIn()
        if (blobNumRemain > 0) {
          numBlobs            = bufIn6.buf(blobNumOff)
          if (blobData == null || blobData.length < numBlobs) {
            blobData = new Array[BlobData](numBlobs)
          }
          blobNumRemain      -= 1
          blobNumOff         += 1
          blobsRead           = 0
          _stateReadBlobNum   = false
          _stateReadBlobData  = true
          stateChange         = true
        } else if (blobNumEnded) {
          _stateComplete      = true
          return stateChange
        }
      }

      if (_stateReadBlobData) {
        if (canReadBlobData) readBlobDataIn()
        val chunk = math.min(blobDataRemain, numBlobs - blobsRead)
        if (chunk > 0) {
          var i = 0
          while (i < chunk) {
            val xMin         = bufIn7 .buf(blobDataOff)
            val xMax         = bufIn8 .buf(blobDataOff)
            val yMin         = bufIn9 .buf(blobDataOff)
            val yMax         = bufIn10.buf(blobDataOff)
            val blob         = BlobData(xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax)
            blobData(blobsRead) = blob
            blobDataRemain  -= 1
            blobDataOff     += 1
            blobsRead       += 1
            i += 1
          }
          stateChange = true
        }
        if (blobsRead == numBlobs) {
          _stateReadBlobData  = false
          _stateProcessBlobs  = true
          stateChange         = true

        } else if (blobDataRemain == 0 && blobDataEnded) {
          _stateComplete      = true
          return stateChange
        }
      }

      if (_stateProcessBlobs) {
        readFromWinRemain   = processWindow(writeToWinOff = writeToWinOff) // , flush = flushIn)
        writeToWinOff       = 0
        readFromWinOff      = 0
        isNextWindow        = true
        auxInOff           += 1
        auxInRemain        -= 1
        _stateProcessBlobs  = false
        stateChange         = true
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
      val oldSize = winSize
      val inOff   = auxInOff
      if (bufIn1 != null && inOff < bufIn1.size) {
        width = math.max(1, bufIn1.buf(inOff))
      }
      if (bufIn2 != null && inOff < bufIn2.size) {
        height = bufIn2.buf(inOff)
      }
      if (bufIn3 != null && inOff < bufIn3.size) {
        minWidth = bufIn3.buf(inOff)
      }
      if (bufIn4 != null && inOff < bufIn4.size) {
        minHeight = bufIn4.buf(inOff)
      }
      if (bufIn5 != null && inOff < bufIn5.size) {
        voices = bufIn5.buf(inOff)
      }
      winSize = width * height
      if (winSize != oldSize) {
        winBuf = new Array[Double](winSize)
      }
      winSize
    }

    private def copyInputToWindow(writeToWinOff: Int, chunk: Int): Unit =
      Util.copy(bufIn0.buf, mainInOff, winBuf, writeToWinOff, chunk)

    private def copyWindowToOutput(readFromWinOff: Int, outOff: Int, chunk: Int): Unit =
      Util.copy(winBuf, readFromWinOff, bufOut0.buf, outOff, chunk)

    private def processWindow(writeToWinOff: Int): Int = {
      val writeOffI = writeToWinOff.toInt
      if (writeOffI == 0) return writeToWinOff

      ???

      val b = winBuf

      writeToWinOff
    }
  }
}
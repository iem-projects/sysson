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

import akka.stream.{Attributes, FanInShape11}
import de.sciss.fscape.stream.impl.{DemandChunkImpl, DemandFilterLogic, DemandInOutImpl, NodeImpl, Out1DoubleImpl, Out1LogicImpl, StageImpl}

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

  private final class Logic(shape: Shape)(implicit ctrl: Control)
    extends NodeImpl(name, shape)
      with DemandFilterLogic[BufD, Shape]
      with DemandChunkImpl  [Shape]
      with Out1LogicImpl    [BufD, Shape]
      with DemandInOutImpl  [Shape]
      with Out1DoubleImpl   [Shape] {

    private[this] var winSize           = 0
    private[this] var lo                = 0.0
    private[this] var hi                = 0.0
    private[this] var lag               = 0.0
    private[this] var winBuf: Array[Double] = _

    private[this] var init              = true
    private[this] var minMem            = 0.0
    private[this] var maxMem            = 0.0
    private[this] var mul               = 0.0
    private[this] var add               = 0.0

    private[this] var writeToWinOff     = 0L
    private[this] var writeToWinRemain  = 0L
    private[this] var readFromWinOff    = 0L
    private[this] var readFromWinRemain = 0L
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

    private[this] var _mainCanRead  = false
    private[this] var _auxCanRead   = false
    private[this] var _mainInValid  = false
    private[this] var _auxInValid   = false
    private[this] var _inValid      = false

    @inline
    private[this] def canWriteToWindow  = readFromWinRemain == 0 && inValid
    
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
      winBuf = null
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

    private def freeInputBuffers(): Unit = {
      freeMainInBuffers()
      freeAuxInBuffers()
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

        val chunk     = math.min(writeToWinRemain, mainInRemain).toInt
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
            readFromWinRemain = processWindow(writeToWinOff = writeToWinOff) // , flush = flushIn)
            writeToWinOff     = 0
            readFromWinOff    = 0
            isNextWindow      = true
            stateChange       = true
            auxInOff         += 1
            auxInRemain      -= 1
            // logStream(s"processWindow(); readFromWinRemain = $readFromWinRemain")
          }
        }
      }

      if (readFromWinRemain > 0) {
        val chunk = math.min(readFromWinRemain, outRemain).toInt
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

    protected def shouldComplete(): Boolean = inputsEnded && writeToWinOff == 0 && readFromWinRemain == 0

    private def startNextWindow(): Long = {
      val oldSize = winSize
      val inOff   = auxInOff
      if (bufIn1 != null && inOff < bufIn1.size) {
        winSize = math.max(1, bufIn1.buf(inOff))
      }
      if (bufIn2 != null && inOff < bufIn2.size) {
        lo = bufIn2.buf(inOff)
      }
      if (bufIn3 != null && inOff < bufIn3.size) {
        hi = bufIn3.buf(inOff)
      }
      if (bufIn4 != null && inOff < bufIn4.size) {
        lag = bufIn4.buf(inOff)
      }
      if (bufIn5 != null && inOff < bufIn5.size) {
        lag = bufIn5.buf(inOff)
      }
      if (winSize != oldSize) {
        winBuf = new Array[Double](winSize)
      }
      winSize
    }

    private def copyInputToWindow(writeToWinOff: Long, chunk: Int): Unit =
      Util.copy(bufIn0.buf, mainInOff, winBuf, writeToWinOff.toInt, chunk)

    private def copyWindowToOutput(readFromWinOff: Long, outOff: Int, chunk: Int): Unit = {
      val _add  = add
      val _mul  = mul
      val a     = winBuf
      val b     = bufOut0.buf
      var ai    = readFromWinOff.toInt
      var bi    = outOff
      val stop  = ai + chunk
      while (ai < stop) {
        val x0 = a(ai)
        val y1 = x0 * _mul + _add
        b(bi)  = y1
        ai += 1
        bi += 1
      }
    }

    private def processWindow(writeToWinOff: Long): Long = {
      val writeOffI = writeToWinOff.toInt
      if (writeOffI == 0) return writeToWinOff

      val b         = winBuf
      var min       = b(0)
      var max       = b(0)
      var i         = 1
      while (i < writeOffI) {
        val x = b(i)
        if      (x > max) max = x
        else if (x < min) min = x
        i += 1
      }

      if (init) {
        init = false
        minMem = min
        maxMem = max
      } else {
        val cy = lag
        val cx = 1.0 - math.abs(cy)
        minMem = minMem * cy + min * cx
        maxMem = maxMem * cy + max * cx
      }

      if (minMem == maxMem) {
        mul = 0
        add = lo
      } else {
        mul = (hi - lo) / (maxMem - minMem)
        add = lo - minMem * mul
      }

      writeToWinOff
    }
  }
}
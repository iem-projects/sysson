/*
 *  Turbulence.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package turbulence

import java.awt.{Color, GraphicsEnvironment}
import java.util.concurrent.TimeUnit
import javax.swing.JComponent

import at.iem.sysson
import at.iem.sysson.turbulence.Dymaxion.{Pt3, Polar, DymPt}
import de.sciss.desktop.KeyStrokes
import de.sciss.file._
import de.sciss.lucre.synth.{Synth, Node, Group, Txn}
import de.sciss.mellite.Mellite
import de.sciss.swingplus.CloseOperation
import de.sciss.swingplus.Implicits._
import de.sciss.synth.{SynthGraph, addBefore, addAfter, AddAction}

// it _is_ used
import de.sciss.synth.proc.{SoundProcesses, Code}
import de.sciss.{synth, desktop, numbers, pdflitz, kollflitz}

import ucar.ma2

import scala.swing.event.{MousePressed, MouseReleased, Key, ButtonClicked}
import scala.swing.{GridBagPanel, Action, FlowPanel, BoxPanel, Orientation, ButtonGroup, ToggleButton, Swing, Frame}
import scala.collection.breakOut
import scala.concurrent.stm.{Ref, atomic}
import Swing._

object Turbulence {
  def UseMercator   = false
  def EnablePDF     = false
  def EnterBinaural = true
  def AutoOpen      = true
  def AutoStart     = false

  final case class DymGrid(vx: Int, vyi: Int) {
    def toPoint: DymPt = {
      val vy = vyi * 2 + (vx % 2)
      DymPt(vx, vy)
    }
  }

  final case class Spk(num: Int) extends AnyVal {
    /** Zero-based channel offset */
    def toIndex = num - 1
  }

  /** Maps from (x, y) index with respect to the
    * equilateral triangle grid to speaker channel
    * (1-based offset)
    */
  final val MatrixToChannelMap = Map[DymGrid, Spk](
    DymGrid( 4, 0) -> Spk( 3),
    DymGrid( 6, 0) -> Spk( 5),
    DymGrid( 8, 0) -> Spk( 4),
    DymGrid( 5, 0) -> Spk( 6),
    DymGrid( 7, 0) -> Spk( 7),
    DymGrid( 6, 1) -> Spk( 8),
    DymGrid( 3, 1) -> Spk(21),
    DymGrid( 5, 1) -> Spk(25),
    DymGrid( 7, 1) -> Spk(26),
    DymGrid( 9, 1) -> Spk(33),
    DymGrid(11, 1) -> Spk(41),
    DymGrid(13, 1) -> Spk(45),
    DymGrid( 4, 2) -> Spk(22),
    DymGrid( 6, 2) -> Spk(27),
    DymGrid( 8, 2) -> Spk(34),
    DymGrid(12, 2) -> Spk(42),
    DymGrid( 5, 2) -> Spk(23),
    DymGrid( 7, 2) -> Spk(29),
    DymGrid( 9, 2) -> Spk(35),
    DymGrid(11, 2) -> Spk(43),
    DymGrid( 0, 3) -> Spk(10),
    DymGrid( 2, 3) -> Spk(13),
    DymGrid( 4, 3) -> Spk(17),
    DymGrid( 6, 3) -> Spk(28),
    DymGrid( 8, 3) -> Spk(30),
    DymGrid(10, 3) -> Spk(37),
    DymGrid(12, 3) -> Spk(46),
    DymGrid( 1, 3) -> Spk(11),
    DymGrid( 3, 3) -> Spk(14),
    DymGrid( 5, 3) -> Spk(24),
    DymGrid( 7, 3) -> Spk(31),
    DymGrid( 9, 3) -> Spk(36),
    DymGrid(11, 3) -> Spk(44),
    DymGrid( 2, 4) -> Spk(15),
    DymGrid( 4, 4) -> Spk(18),
    DymGrid(10, 4) -> Spk(38),
    DymGrid( 1, 4) -> Spk(16),
    DymGrid( 3, 4) -> Spk(19),
    DymGrid( 5, 4) -> Spk(20),
    DymGrid( 7, 4) -> Spk(32),
    DymGrid( 9, 4) -> Spk(39),
    DymGrid(11, 4) -> Spk(40)
  )

  final case class LatLon(lat: Double, lon: Double) {
    override def toString = f"[lat: $lat%1.2f, lon: $lon%1.2f]"

    def toPolar: Polar = {
      val theta = (90 - lat).toRadians
      val phi   = ((lon + 360) % 360).toRadians
      Polar(theta, phi)
    }

    def toCartesian: Pt3 = toPolar.toCartesian
  }

  final case class LatLonIdx(latIdx: Int, lonIdx: Int) {
    def toLatLon = LatLon(latitude(latIdx), longitude(lonIdx))
  }

  /** Maps from speaker channel (1-based offset)
    * to (x, y) index with respect to the
    * equilateral triangle grid
    */
  final val ChannelToMatrixMap: Map[Spk, DymGrid] = MatrixToChannelMap.map(_.swap)

  final val Channels: Vec[Spk] = ChannelToMatrixMap.keysIterator.toVector.sortBy(_.num)

  final val ChannelIndices: Vec[Int] = Channels.map(_.toIndex)

  final val NumChannels = ChannelIndices.size

  private val chans = MatrixToChannelMap.valuesIterator.map(_.num).toVector.sorted
  assert(chans == (3 to 8) ++ (10 to 11) ++ (13 to 46), s"ChannelMap does not have expected values: $chans")

  import numbers.Implicits._

  def latitude (idx: Int): Float = idx.linlin(0,  72,  -90.0f,  +90.0f)
  def longitude(idx: Int): Float = idx.linlin(0, 143, -177.5f, +180.0f)

  final val NumLat =  73
  final val NumLon = 144

  final val baseDir   = userHome / "IEM" / "SysSon" / "installation"
  final val audioWork = baseDir / "audio_work"

  /** Maps latitude index to longitude index to dymaxion coordinate. */
  final val LatLonIndicesToDymMap: Map[LatLonIdx, DymPt] = (0 until NumLat).flatMap { latIdx =>
    (0 until NumLon).map { lonIdx =>
      val lon = longitude(lonIdx)
      val lat = latitude (latIdx)
      val dym = Dymaxion.mapLonLat(LatLon(lat = lat, lon = lon))
      LatLonIdx(latIdx, lonIdx) -> dym
    }
  } (breakOut)

  final val LatLonDym: Vec[(LatLonIdx, DymPt)] = LatLonIndicesToDymMap.toVector

  object Radians {
    val North = Radians(math.Pi * 0.5)
  }
  final case class Radians(value: Double) extends AnyVal {
    /** Wraps the value to ensure it lies within -Pi ... +Pi */
    def normalize: Radians = Radians(value.wrap2(math.Pi))

    def - (that: Radians): Radians = Radians(this.value - that.value)
    def + (that: Radians): Radians = Radians(this.value + that.value)

    def angleTo(that: Radians): Radians = {
      val thisN = this.normalize
      val thatN = that.normalize
      (thatN - thisN).normalize
    }
  }

  /** Maps latitude index to longitude index to
    * a pair of dymaxion coordinate and northward direction ("compass")
    */
  final val LatLonIndicesToDymCompassMap: Map[LatLonIdx, (DymPt, Radians)] =
    LatLonIndicesToDymMap.map { case (idx, pt1) =>
      val latS = idx.latIdx - 1
      val latN = idx.latIdx + 1
      val ptS  = if (latS  <  0) DymPt(-99, -99) else LatLonIndicesToDymMap(LatLonIdx(latS, idx.lonIdx))
      val ptN  = if (latN >= 73) DymPt(-99, -99) else LatLonIndicesToDymMap(LatLonIdx(latN, idx.lonIdx))

      val pt1Eq     = pt1.equalize
      val ptSEq     = ptS.equalize
      val ptNEq     = ptN.equalize
      val useSouth  = (pt1Eq distanceTo ptSEq) < (pt1Eq distanceTo ptNEq)
      val pt2Eq     = if (useSouth) ptSEq else ptNEq
      val ang1      = math.atan2(-(pt2Eq.y - pt1Eq.y), pt2Eq.x - pt1Eq.x)
      val ang       = if (useSouth) (ang1 + math.Pi) % (2 * math.Pi) else ang1

      idx -> (pt1, Radians(ang))
    }

  /** Maps from loudspeaker channels to
    * quantized (lat, lon) pairs. Nearest
    * raster points are used except where
    * there are "jumps" in the Dymaxion,
    * in which case we go "away" a little
    * from the gap, so there are no two
    * loudspeakers with the same geo-coordinates.
    */
  final val ChannelToGeoMap: Map[Spk, LatLonIdx] = ChannelToMatrixMap.map { case (spk, dym) =>
    val pt1 = dym.toPoint.equalize
    val nn  = LatLonDym.minBy { case (ll, pt2) =>
      pt1 distanceTo pt2.equalize
    }
    spk -> nn._1
  }

  assert(ChannelToGeoMap.size == 42 && ChannelToGeoMap.valuesIterator.toSet.size == 42)

  // ChannelToGeoMap2.toList.sortBy(_._1.num).map(tup => (tup._1, tup._2.toLatLon)).foreach(println)

  final val VoronoiMap: Map[Spk, Vec[LatLonIdx]] = {
    val norm = ChannelToMatrixMap.map { case (spk, dym) => spk -> dym.toPoint }
    val v: Vec[(Spk, LatLonIdx)] = LatLonDym.map { case (latLonIdx, dym2) =>
      val pt2 = dym2.equalize
      val spk = norm.minBy(_._2.equalize.distanceTo(pt2))._1
      (spk, latLonIdx)
    }
    import kollflitz.Ops._
    v.toMultiMap(_._1)(_._2)
  }

  // maps sensors to (speakers, wired-yes-no)
  final val SensorSpeakers = Vector(
    Spk(11) -> false,
    Spk(15) -> false,
    Spk(14) -> true,
    Spk(18) -> true,
    Spk(24) -> true,
    Spk(31) -> false,
    Spk(36) -> true,
    Spk(38) -> true,
    Spk(44) -> true,
    Spk(22) -> true,
    Spk(23) -> true,
    Spk(27) -> true,
    Spk(29) -> true,
    Spk(34) -> true,
    Spk(35) -> true,
    Spk(43) -> true,
    Spk(42) -> false,
    Spk( 6) -> false,
    Spk( 8) -> true,
    Spk( 7) -> true
  )

  // nominally - without 'non-wired' ones
  final val NumSensors        = SensorSpeakers.size
  final val NumWiredSensors   = SensorSpeakers.count(_._2)

  assert(NumSensors == 20 && NumWiredSensors == 15)


  def channelCrosses(spk: Spk): Vec[(DymPt, Radians)] = VoronoiMap.getOrElse(spk, Vec.empty).map { idx =>
    LatLonIndicesToDymCompassMap(idx)
  }

  def decimate[A](in: Vec[A])(n: Int): Vec[A] = in.iterator.zipWithIndex.collect {
    case (x, i) if i % n == 0 => x
  } .toIndexedSeq

  // don't use `App` because body will not be initialized if we don't use it as main entry
  def main(args: Array[String]): Unit = {
    //    de.sciss.lucre.stm  .showLog = true
    //    de.sciss.lucre.event.showLog = true

    Main.main(args :+ "--mellite-frame")
    val pkg = "at.iem.sysson.turbulence._" :: Nil
    Code.registerImports(Code.Action    .id, pkg)
    Code.registerImports(Code.SynthGraph.id, pkg)
    atomic { implicit itx =>
      implicit val tx = Txn.wrap(itx)
      Sensors.init()
    }

    Swing.onEDT(initViews())
//    if (AutoOpen) {
//      SoundProcesses.scheduledExecutorService.schedule(
//        new Runnable { def run() = Motion.run(start = AutoStart) }, 3, TimeUnit.SECONDS)
//    }
  }

  private lazy val dyn = new DymaxionView

  private def initViews(): Unit = {
    //    dyn.drawImage    = false
    //    dyn.drawSpeakers = false
    // dyn.crosses = decimate(Preparations.dymGridAng.map(decimate(_)(8)))(4).flatten

    // dyn.crosses      = LatLonIndicesToDymCompassMap.valuesIterator.toVector
    // dyn.crosses = channelCrosses(Spk(29)) ++ channelCrosses(Spk(6))
    // dyn.mouseControl = true // false

    lazy val mercator = new MercatorView(dyn)

    val fDyn = new Frame {
      peer.setUndecorated(true)
      contents = new GridBagPanel {
        val cons = new Constraints()
        layout(dyn) = cons
        background = Color.black
      }
      // resizable = false
    }

    val actionFullScreen = Action(null) {
      val screen = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
      val w = if (screen.getFullScreenWindow == null) fDyn.peer else null
      screen.setFullScreenWindow(w)
      if (w == null) {
        // state is messy and often not correctly restored
        fDyn.pack()
        fDyn.centerOnScreen()
      }
    }

    // relax reaction time and coalesce multiple events, so
    // building up the binaural filter isn't done too frequently
    val binauralTimer = new javax.swing.Timer(100, ActionListener { _ =>
      toggleBinaural(on = true)
    })
    binauralTimer.setRepeats(false)

    dyn.peer.getActionMap.put("full-screen", actionFullScreen.peer)
    dyn.peer.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
      .put(KeyStrokes.menu1 + KeyStrokes.shift + Key.F, "full-screen")
    dyn.listenTo(dyn.mouse.clicks)
    dyn.reactions += {
      case _: MousePressed  => binauralTimer.stop()
      case _: MouseReleased => binauralTimer.restart()
    }

    if (EnablePDF) new pdflitz.SaveAction(dyn :: Nil).setupMenu(fDyn)
    // fDyn.pack()
    // fDyn.centerOnScreen()
    // fDyn.open()

    if (UseMercator) new Frame {
      contents  = mercator
      resizable = false
      pack()
      centerOnScreen()
      open()
    }

    new Frame {
      title = "Turbulence"

      private val ggOff   = new ToggleButton("Off")
      private val ggTest  = new ToggleButton("Test")
      private val ggPos   = new ToggleButton("Listener")
      private val bg      = new ButtonGroup(ggOff, ggTest, ggPos)

      listenTo(ggOff )
      listenTo(ggTest)
      listenTo(ggPos )
      bg.select(ggOff)

      reactions += {
        case ButtonClicked(_) =>
          val b = bg.selected.getOrElse(ggOff)
          dyn.mouseControl =
            if      (b == ggTest) DymaxionView.TestSignal
            else if (b == ggPos ) DymaxionView.ListenerPosition
            else                  DymaxionView.Off
      }

      val ggBinaural = new ToggleButton("Binaural Mix") {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) => toggleBinaural(selected)
        }
      }

//      val ggSpkMix = new ToggleButton("AAAA Speakers") {
//        listenTo(this)
//        reactions += {
//          case ButtonClicked(_) => toggleSpeakerMix(selected)
//        }
//      }

      val ggRun = new ToggleButton("Run Installation") {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
//            Motion.instance.foreach { in =>
//              if (selected) in.startGUI() else in.stopGUI()
//            }
        }
      }

      contents = new BoxPanel(Orientation.Vertical) {
        contents += ggRun
        // contents += ggSpkMix
        contents += VStrut(4)
        contents += ggBinaural
        contents += VStrut(4)
        contents += new FlowPanel(ggOff, ggTest, ggPos)
      }
      pack()

      // bounds = new Rectangle(0, desktop.Util.maximumWindowBounds.height - 72, 148, 72)
      location = (0, desktop.Util.maximumWindowBounds.height - size.height)
      this.defaultCloseOperation = CloseOperation.Ignore
      open()

      ggPos.doClick()
      // ggBinaural.doClick()
    }
  }

  private val binGroup  = Ref(Option.empty[Group])
  private val spkSynth  = Ref(Option.empty[Synth])

  private def toggleBinaural(on: Boolean): Unit = {
    val markOpt = dyn.mark
    atomic { implicit itx =>
      implicit val tx = Txn.wrap(itx)
      Mellite.auralSystem.serverOption.foreach { s =>
        binGroup.swap(None)(tx.peer).foreach(_.release())
        if (on) {
          markOpt.foreach { case (pt, ang) =>
            val listener = Binaural.Person(pt, ang)
            val (target, addAction) = spkSynth.get(tx.peer).fold[(Node, AddAction)](s.defaultGroup -> addAfter)(_ -> addBefore)
            val b = Binaural.build(target = target, addAction = addAction, listener)
            binGroup.set(Some(b))(tx.peer)
          }
        }
      }
    }
  }

  private def toggleSpeakerMix(on: Boolean): Unit = {
    atomic { implicit itx =>
      implicit val tx = Txn.wrap(itx)
      Mellite.auralSystem.serverOption.foreach { s =>
        spkSynth.swap(None)(tx.peer).foreach(_.free())
        if (on) {
          val target  = binGroup.get(tx.peer).getOrElse(s.defaultGroup)
          val graph   = SynthGraph {
            import synth._
            import ugen._
            val m = List(
              List(11, 15, 14),
              List(18, 24, 23),
              List(22,  6,  7),
              List( 8, 27, 29),
              List(31, 34, 35),
              List(36, 38, 44)
            )
            val insAll = m.map { case list =>
              val ins = list.map { spk => In.ar(spk - 1) }
              Mix.mono(ins)
            }
            ReplaceOut.ar(0, insAll)
          }
          val syn = Synth(s, graph, nameHint = Some("aaaa"))
          syn.play(target = target, args = Nil, addAction = addAfter, dependencies = Nil)
          spkSynth.set(Some(syn))(tx.peer)
        }
      }
    }
  }

  // ---------------------------------

  def convertViaVoronoi(inF: File, varName: String, outF: File): Unit = {
    import sysson.Implicits._
    import TransformNetcdfFile.{Keep, Create}
    val in = openFile(inF)
    println(in.variableMap(varName).dimensions.map(_.name))
    try {
      val spkData = ma2.Array.factory(Turbulence.Channels.map(_.num)(breakOut): Array[Int])
      TransformNetcdfFile(in, outF, varName, Vec("lat", "lon"), Vec(Keep("time"), Create("spk", None, spkData))) {
        case (origin, arr) =>
          val dIn   = arr.copyToNDJavaArray().asInstanceOf[Array[Array[Float]]]
          val dOut: Array[Float] = Channels.map { spk =>
            val latLonIndices = VoronoiMap(spk)
            val sum = (0.0 /: latLonIndices) { case (n, idx) =>
              n + dIn(idx.latIdx)(idx.lonIdx)
            }
            val mean = sum / latLonIndices.size
            mean.toFloat
          } (breakOut)
          ma2.Array.factory(dOut)
      }
    } finally {
      in.close()
    }
  }
}

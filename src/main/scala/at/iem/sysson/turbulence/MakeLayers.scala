/*
 *  MakeLayers.scala
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

package at.iem.sysson.turbulence

import at.iem.sysson
import at.iem.sysson.WorkspaceResolver
import at.iem.sysson.gui.impl.DataSourceElem
import at.iem.sysson.sound.Sonification
import at.iem.sysson.turbulence.Turbulence.Spk
import de.sciss.file._
import de.sciss.lucre.artifact.ArtifactLocation
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.{String => StringEx}
import de.sciss.lucre.matrix.{Dimension, Reduce, Matrix, DataSource}
import de.sciss.lucre.stm.TxnLike
import de.sciss.mellite.{Workspace, ObjectActions}
import de.sciss.synth
import de.sciss.synth.io.AudioFile
import de.sciss.synth.{GE, proc, SynthGraph}
import de.sciss.synth.proc.{ArtifactLocationElem, FolderElem, Folder, ObjKeys, StringElem, ExprImplicits, Obj, Proc, graph}
import proc.Implicits._
import ucar.nc2.time.{CalendarDateFormatter, CalendarPeriod}

import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}

import VoiceStructure.{NumChannels, NumLayers}

object MakeLayers {
  final val DataAmonHist_Tas  = ("tas_amon_join_voronoi", "tas")
  final val DataAmonHist_Pr   = ("pr_amon_join_voronoi" , "pr" )
  final val DataTaAnom        = ("ta_anom_spk"          , "Temperature")
  final val DataPrBlog        = ("pr_amon_join_blob"    , "pr")
  final val DataEastWind      = ("ZON_200hPa_ua_amon_join", "ua")

  final val NumBlobs = 4

  final val VerifyBounds  = false
  final val VerifyNaNs    = false

  final val DebugSpeed    = false

  lazy val all: Vec[LayerFactory] = {
    //              0          1             2             3       4           5            6               7
    val res = Vec(Freesound, VoronoiPitch, VoronoiPaper, TaAnom, ConvPrecip, PrecipBlobs, RadiationBlips, Wind,
      new PlaceHolder(8))
    assert(res.map(_.identifier) == (0 until NumLayers))
    res
  }

  /** Maps sensor-ids (counting from 0 until num-wired) to layer-ids. */
  lazy val map: Map[Int, Int] = {
    // first-loop counter-clockwise:  0,  1, 2, 7,  6
    // second-loop                 : 14, 13, 8, 9, 10
    // third-loop                  : 11,  3, 4, 5, 12

    val res = Map( 0 -> 1,  1 -> 2,  2 -> 3,  7 -> 4,  6 -> 5,
                  14 -> 6, 13 -> 7,  8 -> 8,  9 -> 1, 10 -> 2,
                  11 -> 3,  3 -> 4,  4 -> 5,  5 -> 6, 12 -> 7)
    assert(res.size == Turbulence.NumWiredSensors)
    res
  }

  /** Retrieves or creates (if not found) the `"data"` folder in the workspace root. */
  def getDataFolder[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): Folder[S] = {
    val r = workspace.root
    (r / "data").fold[Folder[S]] {
      val f     = Folder[S]
      val fObj  = Obj(FolderElem(f))
      fObj.name = "data"
      r.addLast(fObj)
      f
    } {
      case FolderElem.Obj(f) => f.elem.peer
    }
  }

  /** Retrieves or creates (if not found) the `"base"` location in the data folder of the workspace. */
  def getBaseLocation[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): ArtifactLocation.Modifiable[S] = {
    val data = getDataFolder()
    (data / "base").fold[ArtifactLocation.Modifiable[S]] {
      val loc       = ArtifactLocation[S](Turbulence.baseDir)
      val locObj    = Obj(ArtifactLocationElem(loc))
      locObj.name   = "base"
      data.addLast(locObj)
      loc
    } {
      case ArtifactLocationElem.Obj(locObj) => locObj.elem.peer.modifiableOption.get
    }
  }

  def getMatrix[S <: Sys[S]](nameVar: (String, String))
                            (implicit tx: S#Tx, workspace: Workspace[S]): Matrix[S] = {
    val (name, variable) = nameVar
    val data    = getDataFolder[S]()
    val ds = (data / name).fold[DataSource[S]] {
      val loc     = getBaseLocation()
      val art     = loc.add(Turbulence.dataDir / s"$name.nc")
      implicit val resolver = WorkspaceResolver[S]
      val ds0     = DataSource(art)
      val dsObj   = Obj(DataSourceElem(ds0))
      dsObj.name  = name
      data.addLast(dsObj)
      ds0
    } {
      case DataSourceElem.Obj(dsObj) => dsObj.elem.peer
    }

    val m = ds.variables.find(_.name == variable).get
    m
  }

  def getSonificationSource[S <: Sys[S]](nameVar: (String, String))
                                (implicit tx: S#Tx, workspace: Workspace[S]): Sonification.Source[S] = {
    val (name, variable) = nameVar
    val data    = getDataFolder[S]()
    val ds = (data / name).fold[DataSource[S]] {
      val loc     = getBaseLocation()
      val art     = loc.add(Turbulence.dataDir / s"$name.nc")
      implicit val resolver = WorkspaceResolver[S]
      val ds0     = DataSource(art)
      val dsObj   = Obj(DataSourceElem(ds0))
      dsObj.name  = name
      data.addLast(dsObj)
      ds0
    } {
      case DataSourceElem.Obj(dsObj) => dsObj.elem.peer
    }

    val m = ds.variables.find(_.name == variable).get
    Sonification.Source(m)
  }

  def mkSonif[S <: Sys[S]](name: String)(graphFun: => Unit)(implicit tx: S#Tx): Sonification.Obj[S] = {
    val imp     = ExprImplicits[S]
    import imp._
    val son     = Sonification[S]
    val sonObj  = Obj(Sonification.Elem(son))
    val nameObj = Obj(StringElem(StringEx.newConst[S](name)))
    val pObj    = son.proc
    sonObj.attr.put(ObjKeys.attrName, nameObj)
    pObj  .attr.put(ObjKeys.attrName, nameObj)
    val p       = pObj.elem.peer
    p.graph()   = SynthGraph {
      graphFun
    }
    sonObj
  }

  private def mkTimeRecord(time: GE): Unit = {
    import synth._; import ugen._; import proc.graph._
    Reaction(Impulse.kr(1.0), time, "time")
  }

  def mkVerifyNaNs(sig: GE, id: Int): Unit =
    if (VerifyNaNs) {
      import synth._; import ugen._
      CheckBadValues.ar(sig, id = id)
      sig.poll(sig.abs > 10, s"SPIKE $id")
    }

  // -------------------- Freesound --------------------

  /** the field recording based layer, using
    * geo-tagged sound files from Freesound.org.
    * sound-file names contain ID and consequently resolve author on Freesound.
    */
  object Freesound extends Timeless {
    final val varName = "geo-tag"

    final val identifier = 0

    //    // spk-num to avg energy in decibels
    //    val energy = Map[Int, Double](
    //       3 -> -42.5,  4 -> -33.4,  5 -> -28.9,  6 -> -30.5,  7 -> -36.7,  8 -> -31.8,
    //      10 -> -29.9, 11 -> -28.9, 13 -> -39.5, 14 -> -33.6, 15 -> -38.7, 16 -> -34.3,
    //      17 -> -32.0, 18 -> -37.1, 19 -> -34.5, 20 -> -34.6, 21 -> -49.1, 22 -> -34.7,
    //      23 -> -47.4, 24 -> -41.4, 25 -> -35.8, 26 -> -29.5, 27 -> -24.9, 28 -> -26.9,
    //      29 -> -50.3, 30 -> -34.6, 31 -> -37.3, 33 -> -19.7, 34 -> -43.3, 35 -> -34.1,
    //      36 -> -24.1, 38 -> -37.6, 40 -> -40.0, 41 -> -45.4, 42 -> -30.0, 43 -> -29.3,
    //      44 -> -42.7, 45 -> -28.6, 46 -> -45.9
    //    )

    //    val gains = energy.map { case (k, v) => k -> (-36 - v/2) } .toIndexedSeq.sorted.map {
    //      case (k, v) => f"$k -> $v%1.1f" } .mkString("Map[Int, Double](", ", ", ")")

    //    val gains = Map[Int, Double](
    //       3 -> -14.8,  4 -> -19.3,  5 -> -21.6,  6 -> -20.8,  7 -> -17.7,  8 -> -20.1,
    //      10 -> -21.1, 11 -> -21.6, 13 -> -16.3, 14 -> -19.2, 15 -> -16.7, 16 -> -18.9,
    //      17 -> -20.0, 18 -> -17.5, 19 -> -18.8, 20 -> -18.7, 21 -> -11.5, 22 -> -18.7,
    //      23 -> -12.3, 24 -> -15.3, 25 -> -18.1, 26 -> -21.3, 27 -> -23.6, 28 -> -22.6,
    //      29 -> -10.9, 30 -> -18.7, 31 -> -17.4, 33 -> -26.2, 34 -> -14.4, 35 -> -19.0,
    //      36 -> -24.0, 38 -> -17.2, 40 -> -16.0, 41 -> -13.3, 42 -> -21.0, 43 -> -21.4,
    //      44 -> -14.6, 45 -> -21.7, 46 -> -13.1)

    val gains = Map[Int, Double](
       5 -> -13.3, 10 -> -17.2, 42 -> -16.3, 24 -> -12.3, 37 -> -17.5, 25 -> -10.9,
      14 -> -14.4, 20 -> -18.1, 29 -> -21.6,  6 -> -21.4, 38 -> -16.7, 21 -> -11.5,
      33 -> -20.0, 13 -> -26.2, 41 -> -21.1, 32 -> -21.3, 34 -> -22.6, 17 -> -14.8,
      22 -> -18.7, 44 -> -18.9, 27 -> -17.4, 12 -> -16.0,  7 -> -13.1, 39 -> -19.2,
       3 -> -21.7, 35 -> -15.3, 18 -> -20.8, 16 -> -24.0, 31 -> -17.7, 43 -> -21.6,
      40 -> -18.8, 26 -> -18.7, 23 -> -23.6,  8 -> -14.6, 36 -> -18.7, 30 -> -19.3,
      19 -> -20.1,  4 -> -21.0, 15 -> -19.0)

    val totalGain = -6

    def fileMap: Map[Int, File] = {
      val dir = Turbulence.audioWork / "fsm"  // selected sounds, looped and mono
      // key = speaker-num
      val files: Map[Int, File] = dir.children(_.ext.toLowerCase == "aif").map { f =>
          val n   = f.name
          val i   = n.indexOf('_')
          val id  = n.substring(0, i).toInt
          id -> f
        } (breakOut)
      files
    }

    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc.Obj[S]) = {
      val files = fileMap

      val imp = ExprImplicits[S]
      import imp._

      def mkKey(num: Int) = s"file$num"

      val proc    = Proc[S]
      val procObj = Obj(Proc.Elem(proc))
      proc.graph() = SynthGraph {
        import synth._
        import ugen._

        mkTimeRecord(0)

        val sig = Vec.tabulate(NumChannels) { ch =>
          val spk = Turbulence.Channels(ch)
          // not all chans do have files
          files.get(spk.num).fold[GE](DC.ar(0)) { f =>
            // val amp = graph.Attribute.ir("gain", 0.5)
            val amp = (gains(spk.num) + totalGain).dbamp
            graph.DiskIn.ar(mkKey(spk.num), loop = 1) * amp
          }
        }
        graph.ScanOut(sig)
      }

      val loc = getBaseLocation()

      files.foreach { case (num, f) =>
        val spec    = AudioFile.readSpec(f)
        require(spec.numChannels == 1, s"File $f should be mono, but has ${spec.numChannels} channels")
        val artObj  = ObjectActions.mkAudioFile(loc, f, spec)
        procObj.attr.put(mkKey(num), artObj)
        // import numbers.Implicits._
        // val gain    = (gains(num) + totalGain).dbamp
        // procObj.attr.put("gain", Obj(DoubleElem(gain)))
      }

      (procObj, procObj)
    }
  }

  // -------------------- VoronoiPitch --------------------

  object VoronoiPitch extends Base1850 {
    final val varName = "tas"

    final val identifier = 1

    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc.Obj[S]) = {
      val imp = ExprImplicits[S]
      import imp._

      val varName = "!1850tas"

      val sonObj  = mkSonif[S]("voronoi-pitch") {
        import synth._; import ugen._; import proc.graph._; import sysson.graph._

        val v       = Var(varName)
        val dTime   = Dim(v, "time")
        // val dSpk  = Dim(v, "spk" )

        // val speed   = Attribute.kr("speed", 0.1)
        val speed   = mkSpeed(0.1)

        val time    = dTime.play(speed)
        mkTimeRecord(time)
        val data    = v.play(time)

        val period  = speed.reciprocal
        val lag     = Ramp.ar(data, period)

        val amp     = Attribute.kr("gain", 1.5) // UserValue("amp", 1).kr

        //        // for hist dataset:
        //        val minTas  = 231.962
        //        val maxTas  = 304

        // for joined dataset:
        val minTas  = 232.0196
        val maxTas  = 306.7474

        val dustFreq = 10

        if (VerifyBounds) {
          lag.poll((lag < minTas) + (lag > maxTas), "TAS-OFF")
        }

        val freq    = lag.clip(minTas, maxTas).linexp(minTas, maxTas, 10000, 150)

        val sig = Vec.tabulate(NumChannels) { ch =>
          val dust      = Dust.ar(dustFreq)
          val resFreq0  = freq \ ch
          val resFreq   = resFreq0 // Slew.ar(resFreq0, 10000 * 30, 10000 * 30) // c. 30 ms for full range, to avoid Inf/NaN in Resonz
          val amp1      = amp * AmpCompA.kr(resFreq, root = 100)
          val res       = Resonz.ar(dust, freq = resFreq, rq = 0.1) * 10

          mkVerifyNaNs(res, 1000)
          // CheckBadValues.kr(amp1, id = 1001)
          res * amp1
        }
        ScanOut(sig)
      }

      val son     = sonObj.elem.peer
      val sources = son.sources.modifiableOption.get
      val src     = getSonificationSource(DataAmonHist_Tas)
      val dims    = src.dims.modifiableOption.get
      dims.put("time", "time")
      sources.put(varName, src)

      (sonObj, son.proc)
    }
  }

  // -------------------- VoronoiPaper --------------------

  object VoronoiPaper extends Base1850 {
    final val varName = "pr+tas"

    final val identifier = 2

    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc.Obj[S]) = {
      val varNameTas = "!1850tas"
      val varNamePr  = "!1850pr"

      val sonObj  = mkSonif[S]("voronoi-paper") {
        import synth._; import ugen._; import sysson.graph._

        val DEBUG = false

        val vTas      = Var(varNameTas)
        val vPr       = Var(varNamePr )
        val dTimeTas  = Dim(vTas, "time")
        val dTimePr   = Dim(vPr , "time")  // todo - should be able to share

        // val speed     = graph.Attribute.kr("speed", 0.1)
        val speed     = mkSpeed(0.1)
        val timeTas   = dTimeTas.play(speed)
        val timePr    = dTimePr .play(speed)
        val datTas    = vTas.play(timeTas)
        val datPr     = vPr .play(timePr )
        mkTimeRecord(timeTas)

        val amp       = graph.Attribute.kr("gain", 6)

        val period    = speed.reciprocal
        val lagTas    = Ramp.ar(datTas, period)
        val lagPr     = Ramp.ar(datPr , period)

        //        // for historical dataset:
        //        val minTas = 232
        //        val maxTas = 304

        // for joined dataset:
        val minTas  = 232.0196
        val maxTas  = 306.7474

        //        // for historical dataset
        //        val minPr  = 4.6e-7
        //        val maxPr  = 1.9e-4

        // for joined dataset
        val minPr  = 2.4664e-7
        val maxPr  = 2.2184e-4

        if (VerifyBounds) {
          lagTas.poll((lagTas < minTas) + (lagTas > maxTas), "TAS-OFF")
          lagPr .poll((lagPr  < minPr ) + (lagPr  > maxPr ), "PR -OFF")
        }

        val maxDust     = graph.Attribute.kr("dust", 10)  // 5 // 10

        val numSpk      = Turbulence.NumChannels
        val resFreq     = lagTas.clip(minTas, maxTas).linexp(minTas, maxTas, 10000, 150)
        val dustFreq    = lagPr .clip(minPr , maxPr ).linlin(minPr , maxPr ,     0, maxDust)

        val paperBuf    = graph.Buffer("paper")
        val marksBuf    = graph.Buffer("marks")
        val numRegions  = BufFrames.ir(marksBuf) - 1
        val bufSR       = 44100 // BufSampleRate.ir("paper")
        val bufFrames   = BufFrames.ir(paperBuf)

        if (DEBUG) {
          bufFrames .poll(0, "bufFrames" )
          numRegions.poll(0, "numRegions")
        }

        val sig = Vec.tabulate(numSpk) { ch =>
          val dust    = Dust.kr(dustFreq \ ch)
          val index   = TIRand.kr(lo = 0, hi = numRegions - 1, trig = dust)
          val startF  = Index.kr(marksBuf, index)
          val stopF   = Index.kr(marksBuf, index + 1)
          val numF    = stopF - startF
          val pos     = startF / bufFrames   // 0 to 1 normalised
          val dur     = numF   / bufSR
          val grain = GrainBuf.ar(buf = paperBuf, numChannels = 1, trig = dust,
            dur = dur, speed = 1, pos = pos, interp = 1, pan = 0, envBuf = -1, maxGrains = 512)
          val resFreqCh = resFreq \ ch
          val res   = Resonz.ar(grain, freq = resFreq \ ch, rq = 1 /* 0.1 */)

          mkVerifyNaNs(res, 1001)

          if (DEBUG) {
            index.poll(dust, "idx")
            pos  .poll(dust, "pos")
            dur  .poll(dust, "dur")
          }

          val amp1 = amp * AmpCompA.kr(resFreqCh, root = 100)

          res * amp1
        }

        graph.ScanOut(sig)
      }

      val loc         = getBaseLocation[S]()

      val imp = ExprImplicits[S]
      import imp._

      val son         = sonObj.elem.peer
      val fPaper      = Turbulence.audioWork / "DroppingLeaves1SortM.aif"
      val specPaper   = AudioFile.readSpec(fPaper)
      val artPaperObj = ObjectActions.mkAudioFile(loc, fPaper, specPaper)
      son.proc.attr.put("paper", artPaperObj)

      val fMark       = Turbulence.audioWork / "DroppingLeaves1Mark.aif"
      val specMark    = AudioFile.readSpec(fMark)
      val artMarkObj = ObjectActions.mkAudioFile(loc, fMark, specMark)
      son.proc.attr.put("marks", artMarkObj)

      val sources = son.sources.modifiableOption.get
      val srcTas  = getSonificationSource(DataAmonHist_Tas)
      val srcPr   = getSonificationSource(DataAmonHist_Pr )
      val dimsTas = srcTas.dims.modifiableOption.get
      val dimsPr  = srcPr .dims.modifiableOption.get
      dimsTas.put("time", "time")
      sources.put(varNameTas, srcTas)
      dimsPr .put("time", "time")
      sources.put(varNamePr , srcPr )

      (sonObj, son.proc)
    }
  }

  // -------------------- TaAnom --------------------

  object TaAnom extends LayerFactory {
    final val varName = "RO-ta-anom"

    final val identifier = 3

    private val date2001 = CalendarDateFormatter.isoStringToCalendarDate(null, "2001-01-15")

    def updateTime(months: Float)(implicit tx: TxnLike): String = {
      val frames  = months.toInt
      VoiceStructure.currentFrame2001 = frames
      val date    = date2001.add(months, CalendarPeriod.Field.Month)
      val s       = CalendarDateFormatter.toDateString(date)
      // println(s"TaAnom date = $s")
      s
    }

    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc.Obj[S]) = {
      val imp     = ExprImplicits[S]
      import imp._

      val vrName = "!1850anom"

      val sonObj  = mkSonif[S]("ta-anom") {
        import synth._; import ugen._; import sysson.graph._
        val vr        = Var(vrName)
        val dTime     = Dim(vr, "time")

        // val speed     = graph.Attribute.kr("speed", 0.1)
        val speed     = mkSpeed(0.1)
        val time      = dTime.play(speed)
        val dat0      = vr.play(time)
        val period    = speed.reciprocal

        mkTimeRecord(time)

        //        val altCorr: GE = Vector(1.0, 1.0, 1.0, 0.5, 0.5, 0.3333, 0.1429, 0.125, 0.1429, 0.125, 0.1111, 0.1, 0.1429,
        //          0.1111, 0.1, 0.1, 0.25, 0.2, 0.1667, 0.125, 0.25, 0.25, 0.2, 0.1429, 0.1667, 0.1429, 0.125, 0.1, 0.25,
        //          0.2, 0.1667, 0.125, 0.1429, 0.1111, 0.1, 0.1, 0.25, 0.2, 0.1667, 0.125, 0.25, 0.1429)

        //        val altCorr: GE = Vector(1.0000, 1.0000, 1.0000, 0.7071, 0.7071, 0.5774, 0.3780, 0.3536,
        //                                 0.3780, 0.3536, 0.3333, 0.3162, 0.3780, 0.3333, 0.3162, 0.3162,
        //                                 0.5000, 0.4472, 0.4082, 0.3536, 0.5000, 0.5000, 0.4472, 0.3780,
        //                                 0.4082, 0.3780, 0.3536, 0.3162, 0.5000, 0.4472, 0.4082, 0.3536,
        //                                 0.3780, 0.3333, 0.3162, 0.3162, 0.5000, 0.4472, 0.4082, 0.3536,
        //                                 0.5000, 0.3780)

        val altCorr: GE = Vector(0.5000, 0.4472, 0.5000, 0.4082, 0.3780, 0.3536, 0.3780, 0.3333,
                                 0.3162, 0.3162, 0.5000, 0.4472, 0.4082, 0.3536, 1.0000, 0.7071,
                                 0.5774, 0.5000, 0.5000, 0.4472, 0.4472, 0.4082, 0.4082, 0.3780,
                                 0.3536, 0.3162, 1.0000, 1.0000, 0.7071, 0.5000, 0.3780, 0.3780,
                                 0.3536, 0.3162, 0.3333, 0.3333, 0.3536, 0.3162, 0.3780, 0.3780,
                                 0.3536, 0.3162)

        val dat1      = dat0 * altCorr
        val dat       = Ramp.ar(dat1, period)
        val amp       = graph.Attribute.kr("gain", 1.0)

        //        val min = -12.5   // degrees celsius (in data set)
        //        val max = +12.5   // +17.8   // degrees celsius (in data set)
        //        // val mAbs = math.max(math.abs(min), math.abs(max))
        //        // val min1 = (min - max) / 2  // -15.15; make it more symmetric
        val min = -5.5 // -2.4
        val max = +5.5 // +2.4

        val hot   = GrayNoise.ar(0.25)
        val cold  = Dust.ar(SampleRate.ir / 40) // c. 1000 Hz

        val sig = Vec.tabulate(NumChannels) { ch =>
          // val spk = Turbulence.Channels(ch)
          val anom  = dat \ ch
          // val side  = anom < 0  // too cold = 1, too hot = 0

          val dbMin = -36.0 // -40.0

          val aHot  = anom.clip(0, max).linlin(0, max, dbMin, 0).dbamp - dbMin.dbamp // make sure it becomes zero
          val aCold = anom.clip(min, 0).linlin(0, min, dbMin, 0).dbamp - dbMin.dbamp

          val mix   = (hot * aHot + cold * aCold) * amp

          mkVerifyNaNs(mix, 1002)

          //          val amt   = anom.abs.linlin(0, mAbs, -36, 0).dbamp - (-36.dbamp)
          //          val amp1  = amt * amp
          //          Select.ar(side, Seq(hot, cold)) * amp1
          mix
        }
        graph.ScanOut(sig)
      }

      val nameTime  = "Time"  // stupid capitalization !
      val son     = sonObj.elem.peer
      val sources = son.sources.modifiableOption.get
      val m0      = getMatrix(DataTaAnom)
      val m       = Reduce(m0, Dimension.Selection.Name(nameTime), Reduce.Op.Slice(from = 12, to = 143))
      val src     = Sonification.Source(m)
      val dims    = src.dims.modifiableOption.get
      dims.put("time", nameTime)
      sources.put(vrName, src)

      (sonObj, son.proc)
    }
  }

  // -------------------- ConvPrecip --------------------

  object ConvPrecip extends Base1850 {
    final val varName = "pr"

    final val identifier = 4

    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc.Obj[S]) = {
      val imp     = ExprImplicits[S]
      import imp._

      val dir = Turbulence.audioWork / "conv"
      //      val files = Turbulence.Channels.map { case Spk(num) =>
      //        dir / s"fsm${num}conv.aif"
      //      }

      def mkKey(num: Int) = s"file$num"

      val varName = "!1850pr"

      val sonObj  = mkSonif[S]("conv-precip") {
        import synth._; import ugen._; import sysson.graph._

        val vPr       = Var(varName)
        val dTime     = Dim(vPr, "time")

        // val speed     = graph.Attribute.kr("speed", 0.2)
        val speed     = mkSpeed(0.2)
        val time      = dTime .play(speed)
        val datPr     = vPr .play(time )
        mkTimeRecord(time)

        val amp       = graph.Attribute.kr("gain", 1.2)

        val period    = speed.reciprocal
        val lagPr     = Ramp.ar(datPr , period)

      //        // for hist data
      //        val minPr  = 4.6e-7
      //        val maxPr  = 1.9e-4

        // for joined dataset
        val minPr  = 2.4664e-7
        val maxPr  = 2.2184e-4

        if (VerifyBounds) {
          lagPr.poll((lagPr < minPr) + (lagPr > maxPr), "PR-OFF")
        }

        // XXX TODO not all channels have files yet: 32, 37, 39
        // right now, we use 32 = 20, 39 = 40, 37 = 38

        val dbMin = -40.0

        val sig = Turbulence.Channels.zipWithIndex.map { case (Spk(num), ch) =>
          val prc   = lagPr \ ch
          val amp0  = prc.clip(minPr, maxPr).linlin(minPr, maxPr, dbMin, 0).dbamp - dbMin.dbamp
          val amp1  = amp0 * amp
          graph.DiskIn.ar(mkKey(num), loop = 1) * amp1
        }
        graph.ScanOut(sig)
      }

      val loc     = getBaseLocation()
      val son     = sonObj.elem.peer
      val procObj = son.proc

      val sources = son.sources.modifiableOption.get
      val srcPr   = getSonificationSource(DataAmonHist_Pr )
      val dimsPr  = srcPr .dims.modifiableOption.get
      dimsPr .put("time", "time")
      sources.put(varName, srcPr)

      Turbulence.Channels.foreach { case Spk(num) =>
        val f = dir / s"fsm${num}conv.aif"
        val spec    = AudioFile.readSpec(f)
        require(spec.numChannels == 1, s"File $f should be mono, but has ${spec.numChannels} channels")
        val artObj  = ObjectActions.mkAudioFile(loc, f, spec)
        procObj.attr.put(mkKey(num), artObj)
      }

      (sonObj, procObj)
    }
  }

  // -------------------- PrecipBlobs --------------------

  object PrecipBlobs extends Base1850 {
    final val varName = "pr-blobs"

    final val identifier = 5

    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc.Obj[S]) = {
      val imp     = ExprImplicits[S]
      import imp._

      val DEBUG = false

      val varName = "!1850blob"

      val sonObj  = mkSonif[S]("pr-blob") {
        import synth._; import ugen._; import sysson.graph._
        val vr    = Var(varName)
        val dt    = Dim(vr, "time")
        // val speed = graph.Attribute.kr("speed", 1)
        val speed = mkSpeed(1)
        val time  = dt.play(speed)
        val data0 = vr.play(time)
        val data  = A2K.kr(data0)
        mkTimeRecord(time)

        val period = speed.reciprocal

        //        // in hist data
        //        val maxSum = 0.30788
        //        val maxMax = 5.63343e-4

        // in joined data
        val maxSum = 0.32873
        val maxMax = 5.63343e-4

        // val dbMin = -36.0

        val numVoices = NumBlobs * 2

        val amp = graph.Attribute.kr("amp", 2)

        // (sig, x, y)
        def mkVoice(blobID: Int): (GE, GE, GE, GE) = {
          val off   = blobID * 5
          val gate  = data \ (off + 0)
          val x     = data \ (off + 1)
          val y     = data \ (off + 2)
          val sum   = data \ (off + 3)
          val max   = data \ (off + 4)

          if (VerifyBounds) {
            sum.poll(sum > maxSum, "SUM-OFF")
            max.poll(max > maxMax, "MAX-OFF")
          }

          val gateTr    = Trig.kr(gate, ControlDur.ir)
          val toggle    = gateTr + TDelay.kr(gateTr, period)
          val ff        = ToggleFF.kr(toggle)

          def mkCoord(in: GE): GE = {
            val dif0    = in absdif Delay1.kr(in)
            val dif     = Latch.kr(dif0, dif0 > 0)
            val slewRate= dif * speed   // makes Slew work like Ramp
            val inR     = Slew.kr(in, slewRate, slewRate)
            Select.kr(ff, Seq(inR, in))
          }

          val env   = Env.asr(attack = period, release = period, curve = Curve.linear)
          val eg    = EnvGen.kr(env, gate = gate)

          // val amp   = sum.linlin(0, maxSum, dbMin, 0).dbamp - dbMin.dbamp
          // val amp1  = max.linlin(0, maxMax, 0, amp)
          val amp0  = max.clip(0, maxMax).explin(DataSets.PrecipBlobThreshold, maxMax, 0, amp)
          val ctl   = sum.clip(0, maxSum).explin(DataSets.PrecipBlobThreshold, maxSum, 0, 1)
          val amp1  = amp0 / ctl.max(0.1)

          // val sig   = eg * amp1 // PinkNoise.ar(eg * amp1)
          val eg1   = eg * amp1
          // val ctl   = sum / maxSum

          if (DEBUG) {
            amp1.poll(gateTr, s"voice$blobID - amp")
            ctl .poll(gateTr, s"voice$blobID - ctl")
          }

          (eg1, ctl, mkCoord(x), mkCoord(y))
        }

        def calcIndices(xf: GE, yf: GE): ((GE, GE, GE), (GE, GE, GE), (GE, GE, GE)) = {
          val x   = xf.floor
          val u   = xf - x
          val y0  = yf.floor
          val v   = yf - y0

          // note, we add two to avoid problems with negative numbers,
          // but have to take care to subtract that later
          val y   = {
            val cond1 = (y0 % 2) sig_!= (x % 2)
            val cond2 = u + v < 1
            val cond3 = u - v > 0
            val cond  = (cond1 & cond2) | cond3
            y0 + 2 - cond  // if (cond) y0 + 1 else y0 + 2
          }

          val yDiv  = (y / 2).floor - 1 // the minus 1 corrects the offset
          val yOdd  = y % 2
          val yEven = 1 - yOdd
          val xOdd  = x % 2

          val vx1  = x + yOdd
          val vy1i = yDiv + yOdd
          val vy1  = vy1i * 2 + (vx1 % 2)

          val vx2  = x + (yOdd ^ xOdd)
          val vy2i = yDiv + yEven
          val vy2  = vy2i * 2 + (vx2 % 2)

          val vx3  = x + yEven
          val vy3i = yDiv + yOdd
          val vy3  = vy3i * 2 + (vx3 % 2)

          // cf. https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
          def dist(x: GE, y: GE)(lx1: GE, ly1: GE, lx2: GE, ly2: GE): GE = {
            // |Dy x0 - Dx y0 - x1 y2 + x2 y1| / sqrt(Dx.squared + Dy.squared)
            // - we assume a normalized Dx, Dy, and thus drop the division
            // - indeed we know the height is two and divide by it

            val dx = lx2 - lx1
            val dy = ly2 - ly1
            //  (dy * x - dx * y - lx1 * ly2 + lx2 * ly1).abs / 2
            Sum4(dy * x, -dx * y, -lx1 * ly2, lx2 * ly1).abs / 2
          }

          val df = dist(xf, yf) _
          val d1 = df(vx2, vy2, vx3, vy3)
          val d2 = df(vx3, vy3, vx1, vy1)
          val d3 = df(vx1, vy1, vx2, vy2)
          // println(f"d1 = $d1%1.2f, d2 = $d2%1.2f, d3 = $d3%1.2f, sum = ${d1 + d2 + d3}%1.2f")
          val g1 = d1.sqrt
          val g2 = d2.sqrt
          val g3 = d3.sqrt

          ((vx1, vy1i, g1), (vx2, vy2i, g2), (vx3, vy3i, g3))
        }

        val chanBuf = graph.Buffer("chan-map")

        def OutChan(index: GE, in: GE) =
          PanAz.ar(NumChannels, in, pos = index * 2 / NumChannels)

        def mkOutChan(freqCtl: GE, xi: GE, yi: GE, gain: GE): GE = {
          val freq0 = freqCtl.linlin /* exp */(0, 1, 400, 44100 /* SampleRate.ir */ * 0.25)
          val freq  = freq0 * GrayNoise.ar.linexp(-1, 1, 0.5, 2.0)
          val in0   = CuspL.ar(freq, 1.01, 1.91)
          val in    = HPZ1.ar(Resonz.ar(in0, 600) + Resonz.ar(in0, 3000)) // * 4
          val idx   = xi * 5 + yi
          val ch    = Index.kr(chanBuf, idx)
          val chOk  = ch >= 0
          val amp1  = gain * chOk
          val sig   = in * amp1
          OutChan(ch, sig)
        }

        val mix0 = Mix.tabulate(numVoices) { vci =>
          val (env, ctl, xf, yf) = mkVoice(vci)
          val ((vx1, vy1i, g1), (vx2, vy2i, g2), (vx3, vy3i, g3)) =
            calcIndices(xf = xf, yf = yf)
          val sig1 = mkOutChan(freqCtl = ctl, xi = vx1, yi = vy1i, gain = g1 * env)
          val sig2 = mkOutChan(freqCtl = ctl, xi = vx2, yi = vy2i, gain = g2 * env)
          val sig3 = mkOutChan(freqCtl = ctl, xi = vx3, yi = vy3i, gain = g3 * env)

          // sig1 + sig2 + sig3
          Sum3(sig1, sig2, sig3)
        }

        // val mix = CombN.ar(mix0, 0.2, 0.2, 4)
        //        val mix = Mix.fold(mix0, 2) { in =>
        //          AllpassN.ar(in, 0.050, Seq.fill(1 /* NumChannels */)(Rand(0, 0.050)), 1)
        //        }
//        val mix = {
//          // val mix1 = AllpassN.ar(mix0, 0.3162, 0.3162, 4.236068)
//          val dly   = (period * 0.1414).clip(0.05, 0.2)
//          val decay = (period * 4.2361).clip(1.0 , 5.0)
//          val mix2  = AllpassN.ar(mix0, 0.2, dly, decay)
//          mix2
//        }
val mix = FreeVerb.ar(mix0)

        mkVerifyNaNs(mix, 1003)

        graph.ScanOut(mix)

        //        Turbulence.Channels.zipWithIndex.foreach { case (spk, ch) =>
        //          Out.ar(spk.toIndex, mix \ ch)
        //        }
      }

      val loc     = getBaseLocation()
      val son     = sonObj.elem.peer
      val procObj = son.proc

      val sources = son.sources.modifiableOption.get
      val srcPr   = getSonificationSource(DataPrBlog)
      val dimsPr  = srcPr .dims.modifiableOption.get
      dimsPr .put("time", "time")
      sources.put(varName, srcPr)

      val fMap    = DataSets.dataOutDir / "dymgrid-chan-map.aif"
      val spec    = AudioFile.readSpec(fMap)
      val artObj  = ObjectActions.mkAudioFile(loc, fMap, spec)
      procObj.attr.put("chan-map", artObj)

      (sonObj, procObj)
    }
  }

  // precip-blobs:
  // maxSum = 0.29875579476356506
  // maxMax = 5.231246468611062E-4

  // -------------------- RadiationBlips --------------------

  object RadiationBlips extends LayerFactory {
    final val varName = "radiation"

    final val identifier = 6

    def updateTime(days: Float)(implicit tx: TxnLike): String = {
      val frames  = (days / daysPerMonth).toInt
      VoiceStructure.currentFrame1850 = frames
      val date    = date1850.add(days, CalendarPeriod.Field.Day)
      val s       = CalendarDateFormatter.toDateString(date)
      s
    }

    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc.Obj[S]) = {
      val imp     = ExprImplicits[S]
      import imp._

      val vrNames = Vector(
        "hfls", "hfss", "rlds", "rlus", "rsds", "rsus", "rlut", "rsut", "rsdt", "rtmt"
      )

      val sonObj  = mkSonif[S]("radiation") {
        import synth._; import ugen._; import sysson.graph._

        // hfls - Surface Upward Latent   Heat Flux
        // hfss - Surface Upward Sensible Heat Flux
        // rlds - Surface Downwelling Longwave  Radiation
        // rlus - Surface Upwelling   Longwave  Radiation
        // rsds - Surface Downwelling Shortwave Radiation
        // rsus - Surface Upwelling   Shortwave Radiation
        // rlut - TOA Outgoing        Longwave  Radiation
        // rsut - TOA Outgoing        Shortwave Radiation
        // rsdt - TOA Incident        Shortwave Radiation
        // rtmt - Net Downward Flux at Top of Model

        val minMaxMean = Map(
          "hfls" -> ( 83.0965   ,  89.58005  ,  86.584335 ),
          "hfss" -> ( 18.544447 ,  21.011463 ,  19.53071  ),
          "rlds" -> (332.53802  , 358.69363  , 347.64407  ),
          "rlus" -> (390.01828  , 410.90927  , 402.34683  ),
          "rsds" -> (183.77844  , 188.36255  , 185.71547  ),
          "rsus" -> ( 22.443113 ,  25.105354 ,  23.493246 ),
          "rlut" -> (234.36313  , 239.54384  , 237.6493   ),
          "rsut" -> ( 99.65853  , 107.35386  , 101.578735 ),
          "rsdt" -> (340.19525  , 340.58527  , 340.39017  ),
          "rtmt" -> ( -1.4149872,   2.4347672,   1.1621329)
        )

        // val speed  = graph.Attribute.kr("speed", 6.0)
        val speed  = mkSpeed(3.0)
        val amp    = graph.Attribute.kr("gain" , 0.08)
        val period = speed.reciprocal

        val (vars, times) = vrNames.map { name =>
          val v    = Var(s"!rad$name")
          val dt   = Dim(v, "time")
          val time = dt.play(speed)
          val vp   = v.play(time)
          val (min, max, _) = minMaxMean(name)

          if (VerifyBounds) {
            vp.poll((vp < min) + (vp > max), s"$name-OFF")
          }

          // val norm0 = vp.clip(min, max).linlin(min, max, 0, 1)
          val norm0 = vp.linlin(min, max, 0, 1)
          val norm1 = Ramp.ar(norm0, period)
          val norm  = norm1.clip(0, 1)

          if (VerifyBounds) {
            norm.poll((norm < 0) + (norm > 1), s"$name-NORM-OFF")
          }

          (norm, time)
        } .unzip

        mkTimeRecord(times.head)

        // hfls - spirals upwards
        // hfss - spirals downwards

        // rlds - spirals upwards
        // rlus - spirals upwards
        // rsds - spirals downwards
        // rsus - spirals downwards

        // rlut - spirals up
        // rsut - spirals down (few spikes in beginning)

        // rsdt - spirals up (rises in ambitus)
        // rtmt - spirals up (then down?)

        val minDensity  =  0.1
        val maxDensity  = 20.0
        val powF        =  0.95   // richer spectrum, not plain whole numbered overtones

        val f0   = 240 // 80      // fundamental frequency
        val p   = vrNames.size    // number of partials per channel
        val sig = Mix.tabulate(p) { i =>
            val trig = vars(i).linexp(0, 1, minDensity, maxDensity)
            val freq = f0 * (i + 1).pow(powF)
            val decF = f0 / freq
            // println(s"i = $i, decF = $decF")

            val dust0 = Dust.kr(trig)
            val spkLat = Turbulence.Channels.zipWithIndex.map { case (spk, ch) =>
              val lli = Turbulence.ChannelToGeoMap(spk)
              val lat = Turbulence.latitude(lli.latIdx)
              (ch, lat)
            }
            val start = spkLat.sortBy(_._2).map(_._1)
            // we spred the pulses across the channels,
            // using a quasi-random-but-not ordering (order by latitudes)
            val dust  = PulseDivider.kr(dust0, NumChannels, start)

            val env = Decay2.kr(dust * amp,
              0.005,        // grain attack time
              Rand(0.25 * 2,0.333 * 2) * decF   // grain decay time
            )
            FSinOsc.ar(freq) * env
          }

        mkVerifyNaNs(sig, 1004)

        graph.ScanOut(sig)

        //        ChannelIndices.zipWithIndex.foreach { case (bus, ch) =>
        //          Out.ar(bus, sig \ ch)
        //        }
      }

      val son     = sonObj.elem.peer
      val procObj = son.proc

      val sources = son.sources.modifiableOption.get
      vrNames.foreach { name =>
        val srcPr   = getSonificationSource(s"avg_${name}_amon_join" -> name)
        val dimsPr  = srcPr .dims.modifiableOption.get
        dimsPr .put("time", "time")
        sources.put(s"!rad$name", srcPr )
      }

      (sonObj, procObj)
    }
  }

  // -------------------- Wind --------------------

  object Wind extends Base1850 {
    final val varName = "ua"

    final val identifier = 7

    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc.Obj[S]) = {
      val imp     = ExprImplicits[S]
      import imp._

      val varName = "!1850ua"

      val sonObj  = mkSonif[S]("eastwind") {
        import synth._; import ugen._; import sysson.graph._

        val latValues = -85.0 to 85.0 by 10.0 // that file has a more coarse raster

        val v       = Var(varName)
        val dt      = Dim(v, "time")
        // val speed   = graph.Attribute.kr("speed", 1)
        val speed   = mkSpeed(1.0)
        val time    = dt.play(speed)
        val data0   = v.play(time)
        val period  = speed.reciprocal
        val data    = Ramp.ar(data0, period)
        mkTimeRecord(time)

        //        // expected from hist data
        //        // meters per second
        //        val minUA = -15.46
        //        val maxUA = +48.88

        // expected from joined data
        // meters per second
        val minUA = -16.22
        val maxUA = +53.55

        val amp   = graph.Attribute.kr("gain", 3)

        val sound = graph.DiskIn.ar("sound", loop = 1)

        // val thresh = UserValue("thresh", 2).kr
        // val thresh = graph.Attribute.kr("thresh", 2)

        val sig1: GE = Turbulence.Channels.zipWithIndex.map { case (spk, idx) =>
          val lli    = Turbulence.ChannelToGeoMap(spk)
          val lat    = Turbulence.latitude(lli.latIdx)
          val d      = latValues.map(_ absdif lat)
          val idx    = d.indexOf(d.min)
          val x      = data \ idx

          if (VerifyBounds) {
            x.poll((x < minUA) + (x > maxUA), "UA-OFF")
          }

          //  val thresh = 5.0  // m/s

          // val amt    = (thresh - x.abs).max(0) / thresh // 0 ... 1
          val amt = x.clip(0, maxUA) / maxUA

          val lr     = idx % 2
          val ch     = sound \ lr
          val amp1   = amt * amp
          val sig    = ch * amp1
          sig // Out.ar(spk.toIndex, sig)
        }

        mkVerifyNaNs(sig1, 1005)

        graph.ScanOut(sig1)
      }

      val son     = sonObj.elem.peer
      val procObj = son.proc

      val sources = son.sources.modifiableOption.get
      val srcUA   = getSonificationSource(DataEastWind)
      val dimsUA  = srcUA.dims.modifiableOption.get
      dimsUA .put("time", "time")
      sources.put(varName, srcUA)

      val loc   = getBaseLocation()
      val f     = Turbulence.audioWork / "P22_tree-leaves-rustling.wav"
      val spec  = AudioFile.readSpec(f)
      require(spec.numChannels == 2, s"File $f should be stereo, but has ${spec.numChannels} channels")
      val artObj  = ObjectActions.mkAudioFile(loc, f, spec)
      procObj.attr.put("sound", artObj)

      (sonObj, procObj)
    }
  }

  // -------------------- Placeholder --------------------

  class PlaceHolder(val identifier: Int) extends Timeless {
    final val varName = "" // "none"

    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc.Obj[S]) = {
      val imp = ExprImplicits[S]
      import imp._

      val proc    = Proc[S]
      val procObj = Obj(Proc.Elem(proc))
      proc.graph() = SynthGraph {
        import synth._
        import ugen._
        // val li    = graph.Attribute.ir("li", 0)
        val xy    = SensorIn.kr(offset = 18, numChannels = 2)
        val cx    = (xy \ 0) - 0.5
        val cy    = (xy \ 1) - 0.5
        val ang0  = (cy atan2 cx) / math.Pi   // -pi to +pi  --> -1 to +1
        val ang   = Lag.kr(ang0, 10)
        //        cx .poll(1, "cx")
        //        cy .poll(1, "cy")
        //        ang.poll(1, "ang")

        // val freq  = if (NumLayers == 1) 1000.0: GE else li.linexp(0, NumLayers - 1, 200.0, 4000.0)
        val freq  = ang.clip(-1, 1).linexp(-1, 1, 200.0, 4000.0)
        val amp   = 0.5
        val dust  = Decay.ar(Dust.ar(Seq.fill(NumChannels)(10)), 1).min(1)
        val sig   = Resonz.ar(dust, freq, 0.5) * amp

        mkVerifyNaNs(sig, /* li + */ 2000)

        mkTimeRecord(0)

        graph.ScanOut(sig)
      }

      (procObj, procObj)
    }
  }

  // --------------------------------

  private val date1850 = CalendarDateFormatter.isoStringToCalendarDate(null, "1850-01-01 00:00:00")
  private final val daysPerMonth = 365.2422 / 12  // cf. http://pumas.jpl.nasa.gov/examples/index.php?id=46

  trait Base1850 extends LayerFactory {
    def updateTime(days: Float)(implicit tx: TxnLike): String = {
      val frames  = (days / daysPerMonth).toInt
      VoiceStructure.currentFrame1850 = frames
      val date    = date1850.add(days, CalendarPeriod.Field.Day)
      val s       = CalendarDateFormatter.toDateTimeString(date)
      s
    }
  }

  trait Timeless extends LayerFactory {
    def updateTime(time: Float)(implicit tx: TxnLike): String = ""
  }
}
trait LayerFactory {
  protected def mkSpeed(min: Double): GE = {
    import synth._; import ugen._
    val res = ExpRand(min * 0.05, (min * 10).min(12)).max(min)
    if (MakeLayers.DebugSpeed) res.poll(0, s"li-$identifier speed")
    res
  }

  /** Creates a layer.
    *
    * @param workspace location for finding and adding additional components (if needed)
    * @return a tuple consisting of the container object and the proc whose `"out"`
    *         can should be wired. Often the container object will be the object of that proc.
    */
  def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc.Obj[S])

  def varName: String

  /** Used solely for the visual display. To be consistent with the text. */
  def identifier: Int

  def updateTime(time: Float)(implicit tx: TxnLike): String
}

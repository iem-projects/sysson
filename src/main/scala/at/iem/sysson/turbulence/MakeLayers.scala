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
import de.sciss.mellite.{Workspace, ObjectActions}
import de.sciss.numbers
import de.sciss.synth
import de.sciss.synth.io.AudioFile
import de.sciss.synth.{proc, SynthGraph}
import de.sciss.synth.proc.{ArtifactLocationElem, FolderElem, Folder, ObjKeys, StringElem, DoubleElem, ExprImplicits, Obj, Proc, graph}
import proc.Implicits._

import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}

import VoiceStructure.{NumChannels, NumLayers}

object MakeLayers {
  final val DataAmonHist_Tas  = ("tas_Amon_hist_voronoi", "tas")
  final val DataAmonHist_Pr   = ("pr_Amon_hist_voronoi" , "pr" )
  final val DataTaAnom        = ("ta_anom_spk" , "Temperature")

  lazy val all: Vec[LayerFactory] = {
    val real = Vec(Freesound, VoronoiPitch, VoronoiPaper, TaAnom, ConvPrecip)
    real.padTo(NumLayers, Placeholder)
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

  // -------------------- Freesound --------------------

  /** the field recording based layer, using
    * geo-tagged sound files from Freesound.org.
    * sound-file names contain ID and consequently resolve author on Freesound.
    */
  object Freesound extends LayerFactory {
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

    val gains = Map[Int, Double](
       3 -> -14.8,  4 -> -19.3,  5 -> -21.6,  6 -> -20.8,  7 -> -17.7,  8 -> -20.1,
      10 -> -21.1, 11 -> -21.6, 13 -> -16.3, 14 -> -19.2, 15 -> -16.7, 16 -> -18.9,
      17 -> -20.0, 18 -> -17.5, 19 -> -18.8, 20 -> -18.7, 21 -> -11.5, 22 -> -18.7,
      23 -> -12.3, 24 -> -15.3, 25 -> -18.1, 26 -> -21.3, 27 -> -23.6, 28 -> -22.6,
      29 -> -10.9, 30 -> -18.7, 31 -> -17.4, 33 -> -26.2, 34 -> -14.4, 35 -> -19.0,
      36 -> -24.0, 38 -> -17.2, 40 -> -16.0, 41 -> -13.3, 42 -> -21.0, 43 -> -21.4,
      44 -> -14.6, 45 -> -21.7, 46 -> -13.1)

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

    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc[S]) = {
      val files = fileMap

      val imp = ExprImplicits[S]
      import imp._

      def mkKey(num: Int) = s"file$num"

      val proc    = Proc[S]
      val procObj = Obj(Proc.Elem(proc))
      proc.graph() = SynthGraph {
        import synth._
        import ugen._
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

      (procObj, proc)
    }
  }

  // -------------------- VoronoiPitch --------------------

  object VoronoiPitch extends LayerFactory {
    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc[S]) = {
      val imp = ExprImplicits[S]
      import imp._

      val sonObj  = mkSonif[S]("voronoi-pitch") {
        import synth._; import ugen._; import proc.graph._; import sysson.graph._

        val v       = Var("tas")
        val dTime   = Dim(v, "time")
        // val dSpk  = Dim(v, "spk" )

        val speed   = Attribute.kr("speed", 0.1)
        val time    = dTime.play(speed)
        val data    = v.play(time)

        val period  = speed.reciprocal
        val lag     = Ramp.ar(data, period)

        val amp     = Attribute.kr("gain", 1.5) // UserValue("amp", 1).kr

        // for historical dataset:
        val minTas  = 232
        val maxTas  = 304

        val dustFreq = 10

        val freq    = lag.linexp(minTas, maxTas, 10000, 100)

        val sig = Vec.tabulate(NumChannels) { ch =>
          val dust    = Dust.ar(dustFreq)
          val resFreq = freq \ ch
          val amp1    = amp * AmpCompA.kr(resFreq, root = 100)
          val res     = Resonz.ar(dust, freq = resFreq, rq = 0.1) * 10
          res * amp1
        }
        ScanOut(sig)
      }

      val son     = sonObj.elem.peer
      val sources = son.sources.modifiableOption.get
      val src     = getSonificationSource(DataAmonHist_Tas)
      val dims    = src.dims.modifiableOption.get
      dims.put("time", "time")
      sources.put("tas", src)

      (sonObj, son.proc.elem.peer)
    }
  }

  // -------------------- VoronoiPaper --------------------

  object VoronoiPaper extends LayerFactory {
    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc[S]) = {

      val sonObj  = mkSonif[S]("voronoi-paper") {
        import synth._; import ugen._; import sysson.graph._

        val DEBUG = false

        val vTas      = Var("tas")
        val vPr       = Var("pr" )
        val dTimeTas  = Dim(vTas, "time")
        val dTimePr   = Dim(vPr , "time")  // XXX TODO - should be able to share

        val speed     = graph.Attribute.kr("speed", 0.1)
        val timeTas   = dTimeTas.play(speed)
        val timePr    = dTimePr .play(speed)
        val datTas    = vTas.play(timeTas)
        val datPr     = vPr .play(timePr )

        val amp       = graph.Attribute.kr("gain", 6)

        val period    = speed.reciprocal
        val lagTas    = Ramp.ar(datTas, period)
        val lagPr     = Ramp.ar(datPr , period)

        // for historical dataset:
        val minTas = 232
        val maxTas = 304

        val minPr  = 4.6e-7
        val maxPr  = 1.9e-4

        val maxDust     = graph.Attribute.kr("dust", 10)  // 5 // 10

        val numSpk      = Turbulence.NumChannels
        val resFreq     = lagTas.linexp(minTas, maxTas, 10000, 100)
        val dustFreq    = lagPr .linlin(minPr , maxPr ,     0, maxDust)

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
      sources.put("tas" , srcTas)
      dimsPr .put("time", "time")
      sources.put("pr"  , srcPr )

      (sonObj, son.proc.elem.peer)
    }
  }

  // -------------------- TaAnom --------------------

  object TaAnom extends LayerFactory {
    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc[S]) = {
      val imp     = ExprImplicits[S]
      import imp._

      val sonObj  = mkSonif[S]("ta-anom") {
        import synth._; import ugen._; import sysson.graph._
        val vr        = Var("anom")
        val dTime     = Dim(vr, "time")

        val speed     = graph.Attribute.kr("speed", 0.1)
        val time      = dTime.play(speed)
        val dat0      = vr.play(time)
        val period    = speed.reciprocal

        //        val altCorr: GE = Vector(1.0, 1.0, 1.0, 0.5, 0.5, 0.3333, 0.1429, 0.125, 0.1429, 0.125, 0.1111, 0.1, 0.1429,
        //          0.1111, 0.1, 0.1, 0.25, 0.2, 0.1667, 0.125, 0.25, 0.25, 0.2, 0.1429, 0.1667, 0.1429, 0.125, 0.1, 0.25,
        //          0.2, 0.1667, 0.125, 0.1429, 0.1111, 0.1, 0.1, 0.25, 0.2, 0.1667, 0.125, 0.25, 0.1429)
        val altCorr: GE = Vector(1.0000, 1.0000, 1.0000, 0.7071, 0.7071, 0.5774, 0.3780, 0.3536,
                                 0.3780, 0.3536, 0.3333, 0.3162, 0.3780, 0.3333, 0.3162, 0.3162,
                                 0.5000, 0.4472, 0.4082, 0.3536, 0.5000, 0.5000, 0.4472, 0.3780,
                                 0.4082, 0.3780, 0.3536, 0.3162, 0.5000, 0.4472, 0.4082, 0.3536,
                                 0.3780, 0.3333, 0.3162, 0.3162, 0.5000, 0.4472, 0.4082, 0.3536,
                                 0.5000, 0.3780)

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

          val dbMin = -40.0

          val aHot  = anom.clip(0, max).linlin(0, max, dbMin, 0).dbamp - dbMin.dbamp // make sure it becomes zero
          val aCold = anom.clip(min, 0).linlin(0, min, dbMin, 0).dbamp - dbMin.dbamp

          val mix   = (hot * aHot + cold * aCold) * amp

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
      sources.put("anom" , src)

      (sonObj, son.proc.elem.peer)
    }
  }

  // -------------------- ConvPrecip --------------------

  object ConvPrecip extends LayerFactory {
    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc[S]) = {
      val imp     = ExprImplicits[S]
      import imp._

      val dir = Turbulence.audioWork / "conv"
      //      val files = Turbulence.Channels.map { case Spk(num) =>
      //        dir / s"fsm${num}conv.aif"
      //      }

      def mkKey(num: Int) = s"file$num"

      val sonObj  = mkSonif[S]("conv-precip") {
        import synth._; import ugen._; import sysson.graph._

        val vPr       = Var("pr" )
        val dTimePr   = Dim(vPr , "time")

        val speed     = graph.Attribute.kr("speed", 0.2)
        val timePr    = dTimePr .play(speed)
        val datPr     = vPr .play(timePr )

        val amp       = graph.Attribute.kr("gain", 1.2)

        val period    = speed.reciprocal
        val lagPr     = Ramp.ar(datPr , period)

        val minPr  = 4.6e-7
        val maxPr  = 1.9e-4

        // XXX TODO not all channels have files yet: 32, 37, 39
        // right now, we use 32 = 20, 39 = 40, 37 = 38

        val dbMin = -40.0

        val sig = Turbulence.Channels.zipWithIndex.map { case (Spk(num), ch) =>
          val prc   = lagPr \ ch
          val amp0  = prc.linlin(minPr, maxPr, dbMin, 0).dbamp - dbMin.dbamp
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
      sources.put("pr"  , srcPr )

      Turbulence.Channels.foreach { case Spk(num) =>
        val f = dir / s"fsm${num}conv.aif"
        val spec    = AudioFile.readSpec(f)
        require(spec.numChannels == 1, s"File $f should be mono, but has ${spec.numChannels} channels")
        val artObj  = ObjectActions.mkAudioFile(loc, f, spec)
        procObj.attr.put(mkKey(num), artObj)
      }

      (sonObj, procObj.elem.peer)
    }
  }

  // -------------------- PrecipBlobs --------------------

  object PrecipBlobs extends LayerFactory {
    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc[S]) = {
      ???
    }
  }

  // precip-blobs:
  // maxSum = 0.29875579476356506
  // maxMax = 5.231246468611062E-4


  /*
  // graph function source code

val v       = Var("ta")
val dimPres = Dim(v, "pres")
val dimTime = Dim(v, "time")
val dimLat  = Dim(v, "lat" )

val normBuf = Buffer("norm")
val presBuf = Buffer("pres")

val tempo   = UserValue("speed", 1).kr
val timeP   = dimTime.play(tempo)
val ta      = v.play(timeP)

val presVals = ta.axis(dimPres).values
val latVals  = ta.axis(dimLat ).values

def presIdx(pres: GE) = DetectIndex.ar(presBuf, pres)
def latIdx (lat : GE) = lat.linlin(-85, 85, 0, 17)

// time dimension unit is already months!
// thus the month in a year is the time
// value modulus 12
val month     = timeP % 12
val normIndex = (presIdx(presVals) * 18 + latIdx(latVals)) * 12 + month

val norm    = Index.ar(normBuf, normIndex)

val anom0   = ta - norm
val anom    = Ramp.ar(anom0, tempo.reciprocal)

val trig = Impulse.ar(tempo)  // time-synchronous trigger for general use

// --- debug printing ---
val PRINT = false

if (PRINT) {
  ta.poll  (trig, "ta ")
  normIndex.poll(trig, "idx ")
  norm.poll(trig, "norm")
  anom.poll(trig, "anom")
}

// pressure level range:
// 100 hPa ... 10 hPa
// where 10 hPa highest pitch
//      100 hPa lowest  pitch
// values are roughly logarithmically spaced

val side       = anom < 0  // too cold = 1, too hot = 0
val octave     = side * 2
val pitchRange = K2A.ar(UserValue("pitch-range", 12).kr)

val pitchClass = presVals.max(1.0e-10).explin(10, 100, pitchRange, 0).roundTo(1)
// val pitchClass = presVals.max(1.0e-10).explin(40, 50, pitchRange, 0).roundTo(1)

val pitch = octave * 12 + 60 + pitchClass

if (PRINT) pitch.poll(trig, "pch")

val freq    = pitch.clip(12, 135).midicps
val ampCorr = AmpCompA.ar(freq)
val amp     = (anom.abs / 4) * ampCorr

val ok   = (ta \ 0) > 0
val gain = UserValue("gain", -12).kr.dbamp * ok

val osc = SinOsc.ar(freq) * amp
val sig = Mix(osc) * K2A.ar(gain)

val yearTrig = Trig1.ar(month sig_== 0, dur = SampleDur.ir)
val yearAmp = K2A.ar(UserValue("year-pulse", 0.1).kr)
val yearSig = Decay.ar(yearTrig, 0.1).min(1) * WhiteNoise.ar(yearAmp)

val outSig = sig + yearSig

output := Pan2.ar(outSig)  // TODO: decide panorama / stero image

   */

  // -------------------- Placeholder --------------------

  object Placeholder extends LayerFactory {
    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc[S]) = {
      val imp = ExprImplicits[S]
      import imp._

      val proc    = Proc[S]
      val procObj = Obj(Proc.Elem(proc))
      proc.graph() = SynthGraph {
        import synth._
        import ugen._
        val li    = graph.Attribute.ir("li", 0)
        val freq  = if (NumLayers == 1) 1000.0: GE else li.linexp(0, NumLayers - 1, 200.0, 4000.0)
        val amp   = 0.5
        val dust  = Decay.ar(Dust.ar(Seq.fill(NumChannels)(10)), 1).min(1)
        val sig   = Resonz.ar(dust, freq, 0.5) * amp
        graph.ScanOut(sig)
      }

      (procObj, proc)
    }
  }
}
trait LayerFactory {
  /** Creates a layer.
    *
    * @param workspace location for finding and adding additional components (if needed)
    * @return a tuple consisting of the container object and the proc whose `"out"`
    *         can should be wired. Often the container object will be the object of that proc.
    */
  def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc[S])
}
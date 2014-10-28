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
import at.iem.sysson.sound.Sonification
import de.sciss.file._
import de.sciss.lucre.artifact.ArtifactLocation
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.{String => StringEx}
import de.sciss.lucre.matrix.DataSource
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
  lazy val all: Vec[LayerFactory] = {
    val real = Vec(Freesound, VoronoiPitch)
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

    //    val x = energy.map { case (k, v) => k -> (-36 - v/2) } .toIndexedSeq.sorted.map {
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

    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc[S]) = {
      val dir = Turbulence.audioWork / "fsm"  // selected sounds, looped and mono
      // key = speaker-num
      val files: Map[Int, File] = dir.children(_.ext.toLowerCase == "aif").map { f =>
        val n   = f.name
        val i   = n.indexOf('_')
        val id  = n.substring(0, i).toInt
        id -> f
      } (breakOut)

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
            val amp = graph.Attribute.ir("gain", 0.5)
            graph.DiskIn.ar(mkKey(spk.num), loop = 1) * amp
          }
        }
        graph.ScanOut(sig)
      }

      val loc = getBaseLocation()

      files.foreach { case (num, f) =>
        val spec    = AudioFile.readSpec(f)
        val artObj  = ObjectActions.mkAudioFile(loc, f, spec)
        procObj.attr.put(mkKey(num), artObj)
        import numbers.Implicits._
        val gain    = (gains(num) + totalGain).dbamp
        procObj.attr.put("gain", Obj(DoubleElem(gain)))
      }

      (procObj, proc)
    }
  }

  // -------------------- VoronoiPitch --------------------

  object VoronoiPitch extends LayerFactory {
    def mkLayer[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): (Obj[S], Proc[S]) = {
      val imp = ExprImplicits[S]
      import imp._

      val son     = Sonification[S]
      val sonObj  = Obj(Sonification.Elem(son))
      val name    = Obj(StringElem(StringEx.newConst[S]("voronoi-pitch")))
      val pObj    = son.proc
      sonObj.attr.put(ObjKeys.attrName, name)
      pObj  .attr.put(ObjKeys.attrName, name)
      val p       = pObj.elem.peer

      p.graph() = SynthGraph {
        import synth._; import ugen._; import proc.graph._; import sysson.graph._

        val v       = Var("tas")
        val dTime   = Dim(v, "time")
        // val dSpk  = Dim(v, "spk" )

        val speed   = Attribute.kr("speed", 0.1)
        val time    = dTime.play(speed)
        val data    = v.play(time)

        val period  = speed.reciprocal
        val lag     = Ramp.ar(data, period)

        val amp     = Attribute.kr("gain", 1) // UserValue("amp", 1).kr

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

      implicit val resolver = WorkspaceResolver[S]
      val loc     = getBaseLocation()
      val art     = loc.add(Turbulence.dataDir / "tas_Amon_hist_voronoi.nc")
      val ds      = DataSource(art)
      val sources = son.sources.modifiableOption.get
      val m       = ds.variables.find(_.name == "tas").get
      val src     = Sonification.Source(m)
      val dims    = src.dims.modifiableOption.get
      dims.put("time", "time")
      sources.put("tas", src)

      (sonObj, p)
    }
  }

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
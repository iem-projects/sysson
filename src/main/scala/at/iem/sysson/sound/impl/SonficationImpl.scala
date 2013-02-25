package at.iem.sysson
package sound
package impl

import de.sciss.synth._
import concurrent.duration.Duration
import io.{AudioFileSpec, AudioFile}
import java.io.File
import Implicits._
import Ops._
import de.sciss.osc

object SonficationImpl {
  private final val synthDefName = "$son_play"

  def apply(name: String): Sonification = new Impl(name)

  private final class Impl(var name: String) extends Sonification {
    var graph:    SynthGraph                      = Sonification.emptyGraph
    var matrices: Map[String, MatrixSpec]         = Map.empty
    var mapping:  Map[String, SonificationSource] = Map.empty

    override def toString = s"Sonification($name)${hashCode().toHexString}"

    def playOver(duration: Duration): Synth = {
      val szs = mapping.values.collect {
        case r @ RowSource(_) => r.size
      }
      val sz    = szs.headOption.getOrElse(sys.error("playOver requires at least one row vector"))
      val rate  = sz.toDouble / duration.toMillis
      play(rate = rate)
    }

    def play(rate: Double): Synth = {
      val s = AudioSystem.instance.server match {
        case Some(_s: Server) => _s
        case _ => sys.error("Audio system not running")
      }
      validateMatrixSpecs()
      val (ug, keySet) = UGenGraphBuilderImpl(this)
      val sd      = SynthDef(synthDefName, ug)
      val syn     = Synth(s)
      var ctls    = Vector.empty[ControlSetMap]
      var allocs  = Vector.empty[osc.Message with message.HasCompletion]

      keySet.foreach { key =>
        mapping(key) match {
          case col @ ColumnSource(section) =>
            val colSz     = col.size
            val b         = Buffer(s)
            val file      = File.createTempFile("sysson", ".aif")
            val af        = AudioFile.openWrite(file, AudioFileSpec(numChannels = colSz, sampleRate = 44100))
            val ab        = af.buffer(1)
            val data      = section.read().f1d
            data.zipWithIndex.foreach { case (f, ch) => ab(ch)(0) = f }
            af.write(ab)
            af.close()
            val readMsg   = b.readMsg(file.getAbsolutePath)
            val allocMsg  = b.allocMsg(numFrames = 1, numChannels = colSz)
            syn.onEnd { b.free() }
            allocs :+= allocMsg
            allocs :+= readMsg
            ctls   :+= ((key -> b.id): ControlSetMap)

          case row @ RowSource(section) =>
            ???
        }
      }
      val newMsg  = syn.newMsg(synthDefName, args = ctls)
      val recvMsg = sd.recvMsg(newMsg)
      val bndl    = if (allocs.isEmpty) newMsg else {
        val init = allocs.init
        val last = allocs.last
        val upd  = last.updateCompletion(Some(recvMsg))
        osc.Bundle.now((init :+ upd): _*)
      }

      s ! bndl

      syn
    }

    def _debug_writeDef() {
      val dir     = file(sys.props("user.home")) / "Desktop"
      val (ug, _) = UGenGraphBuilderImpl(this)
      val sd      = SynthDef(synthDefName, ug)
      sd.write(dir.getPath, overwrite = true)
    }

    private def validateMatrixSpecs() {
      mapping.foreach { case (key, source) =>
        val spec = matrices.getOrElse(key, sys.error(s"Sonification contains source for unknown key '$key'"))
        // XXX TODO : spec
      }
    }
  }
}
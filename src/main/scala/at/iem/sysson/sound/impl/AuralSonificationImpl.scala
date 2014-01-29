/*
 *  AuralSonificationImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound
package impl

import de.sciss.lucre.event.Sys
import at.iem.sysson.sound.AuralSonification.{Update, Playing, Stopped, Preparing}
import at.iem.sysson.impl.TxnModelImpl
import scala.concurrent.stm.{TxnExecutor, Txn, TxnLocal, Ref}
import de.sciss.lucre.{synth, stm}
import scala.concurrent.{ExecutionContext, future, blocking}
import de.sciss.synth.proc.{Artifact, Grapheme, Attribute, SynthGraphs, AuralPresentation, Transport, ProcGroup, Proc}
import de.sciss.lucre.synth.expr.{Longs, DoubleVec, Doubles, SpanLikes}
import de.sciss.span.Span
import de.sciss.lucre
import scala.util.control.NonFatal
import de.sciss.file.File
import de.sciss.synth.io.AudioFileSpec

object AuralSonificationImpl {
  private val _current = TxnLocal(Option.empty[AuralSonification[_]])

  private[sysson] def current(): AuralSonification[_] = {
    implicit val tx = Txn.findCurrent.getOrElse(sys.error("Called outside of transaction"))
    _current.get.getOrElse(sys.error("Called outside transport play"))
  }

  private def using[S <: Sys[S], A](aural: AuralSonification[S])(block: => A)(implicit tx: S#Tx): A = {
    val old = _current.swap(Some(aural))(tx.peer)
    val res = block
    _current.set(old)(tx.peer)
    res
  }

  def apply[S <: Sys[S], I <: synth.Sys[I]](aw: AuralWorkspace[S, I], sonification: Sonification[S])
                        (implicit tx: S#Tx): AuralSonification[S] = {
    val w             = aw.workspace
    implicit val itx  = w.inMemoryTx(tx)         // why can't I just import w.inMemory !?
    val sonifH        = tx.newHandle(sonification)
    val proc          = Proc[I]
    val group         = ProcGroup.Modifiable[I]
    val span          = SpanLikes.newVar[I](SpanLikes.newConst(Span.from(0L)))
    group.add(span, proc)
    import w.{inMemoryCursor, inMemoryTx}
    val transport     = Transport[I, I](group)
    val auralSys      = AudioSystem.instance.aural
    val aural         = AuralPresentation.runTx(transport, auralSys)
    new Impl[S, I](aw, aural, sonifH, itx.newHandle(proc), transport)
  }

  // private sealed trait State
  // private case object Stopped extends State

  private final class Impl[S <: Sys[S], I <: lucre.synth.Sys[I]](aw: AuralWorkspace[S, I],
                                                                 ap: AuralPresentation[I],
                                                                 sonifH: stm.Source[S#Tx, Sonification[S]],
      procH: stm.Source[I#Tx, Proc[I]],
      transport: Transport[I, Proc[I], Transport.Proc.Update[I]])(implicit iCursor: stm.Cursor[I], iTx: S#Tx => I#Tx)
    extends AuralSonification[S] with TxnModelImpl[S#Tx, Update] {
    impl =>

    private val _state    = Ref(Stopped: Update)
    private val attrMap   = Ref(Map.empty[Any, String])

    def state(implicit tx: S#Tx): Update = _state.get(tx.peer)

    private def state_=(value: Update)(implicit tx: S#Tx): Unit = {
      val oldState = _state.swap(value)(tx.peer)
      if (oldState != value) dispatch(value)
    }

    def stop()(implicit tx: S#Tx): Unit = {
      state_=(Stopped)
      implicit val itx: I#Tx = iTx(tx)
      transport.stop()
    }

    def play()(implicit tx: S#Tx): Unit = {
      stop()
      prepare()
    }

    def attributeKey(elem: Any): String = TxnExecutor.defaultAtomic { implicit itx =>
      attrMap().getOrElse(elem, sys.error(s"No key for attribute $elem"))
    }

    private def prepare()(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = iTx(tx)

      val sonif = sonifH()
      val g     = sonif.patch.graph.value
      val proc  = procH()

      var aMap  = Map.empty[Any, String]
      var aCnt  = 0

      def addAttr(elem: Any): String = {
        val key = s"sonif_$aCnt"
        aCnt   += 1
        aMap   += elem -> key
        key
      }

      def putAttrValue(key: String, value: Double): Unit =
        proc.attributes.put(key, Attribute.Double(Doubles.newConst(value)))

      def putAttrValues(key: String, values: Vec[Double]): Unit =
        proc.attributes.put(key, Attribute.DoubleVec(DoubleVec.newConst(values)))

      g.sources.foreach {
        case uv: graph.UserValue =>
          val attrKey = addAttr(uv)
          sonif.controls.get(uv.key).foreach { expr =>
            putAttrValue(attrKey, expr.value)
          }

        case dp: graph.Dim.Play =>
          ??? // val key = addAttr(dp)

        case vav: graph.Var.Axis.Values =>
          val attrKey     = addAttr(vav)
          // TODO: review old sections iteration, write audio file (cache!) and place as Attribute.AudioGrapheme
          val dir: File   = ???
          val loc         = Artifact.Location.Modifiable[I](dir)
          val file: File  = ???
          val artifact    = loc.add(file)
          val spec: AudioFileSpec = ???
          val offset      = 0L
          val gain        = 1.0
          // val gv          = Grapheme.Value.Audio(file, spec, offset, gain)
          // val g           = Grapheme.Elem.Audio.newConst[I](gv)
          val g           = Grapheme.Elem.Audio(artifact, spec, Longs.newConst(offset), Doubles.newConst(gain))
          proc.attributes.put(attrKey, Attribute.AudioGrapheme(g))

        case dv: graph.Dim.Values =>
          val attrKey = addAttr(dv)
          val section: VariableSection = ???
          val (g, fut) = aw.graphemeCache(section)
          ??? // proc.attributes.put(attrKey, Attribute.AudioGrapheme(g))

        case _ =>
      }

      attrMap.set(aMap)(tx.peer)
      state_=(Preparing)

      proc.graph() = SynthGraphs.newConst[I](g)

      tx.afterCommit {
        import ExecutionContext.Implicits.global
        future {
          blocking {
            Thread.sleep(2000)
          }
          aw.workspace.cursor.step { implicit tx =>
            implicit val itx: I#Tx = iTx(tx)
            transport.seek(0L)
            try {
              using(impl)(transport.play())
            } catch {
              case NonFatal(foo) =>
              foo.printStackTrace()
              throw foo
            }
            state_=(Playing)
          }
        }
      }
    }
  }
}

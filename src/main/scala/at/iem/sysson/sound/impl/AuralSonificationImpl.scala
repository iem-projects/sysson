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
import scala.concurrent.stm.Ref
import de.sciss.lucre.stm
import scala.concurrent.{ExecutionContext, future, blocking}
import de.sciss.synth.proc.{Attribute, SynthGraphs, AuralPresentation, Transport, ProcGroup, Proc}
import de.sciss.lucre.synth.expr.{Doubles, SpanLikes}
import de.sciss.span.Span
import de.sciss.lucre
import scala.util.control.NonFatal

object AuralSonificationImpl {
  def apply[S <: Sys[S]](aw: AuralWorkspace[S], sonification: Sonification[S])
                        (implicit tx: S#Tx): AuralSonification[S] = {
    val w             = aw.workspace
    implicit val itx  = w.inMemoryTx(tx)         // why can't I just import w.inMemory !?
    val sonifH        = tx.newHandle(sonification)
    val proc          = Proc[w.I]
    val group         = ProcGroup.Modifiable[w.I]
    val span          = SpanLikes.newVar[w.I](SpanLikes.newConst(Span.from(0L)))
    group.add(span, proc)
    import w.inMemorySys
    val transport     = Transport[w.I, w.I](group)
    val auralSys      = AudioSystem.instance.aural
    val aural         = AuralPresentation.runTx(transport, auralSys)
    new Impl[S, w.I](aw, aural, w.inMemorySys, w.inMemoryTx, sonifH, proc, transport)
  }

  // private sealed trait State
  // private case object Stopped extends State

  private final class Impl[S <: Sys[S], I <: lucre.synth.Sys[I]](aw: AuralWorkspace[S], ap: AuralPresentation[I],
                                                          iCursor: stm.Cursor[I], iTx: S#Tx => I#Tx,
                                                          sonifH: stm.Source[S#Tx, Sonification[S]],
      proc: Proc[I], // no handle necessary for in-memory!!
      transport: Transport[I, Proc[I], Transport.Proc.Update[I]])
    extends AuralSonification[S] with TxnModelImpl[S#Tx, Update] {

    private val _state = Ref(Stopped: Update)

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

    private def prepare()(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = iTx(tx)

      val sonif = sonifH()
      val g     = sonif.patch.graph.value
      g.sources.foreach {
        case uv: graph.UserValue =>
          sonif.controls.get(uv.key).foreach { expr =>
            proc.attributes.put(uv.attrKey, Attribute.Double(Doubles.newConst(expr.value)))
          }

        case _ =>
      }
      // XXX TODO: expand
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
              transport.play()
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

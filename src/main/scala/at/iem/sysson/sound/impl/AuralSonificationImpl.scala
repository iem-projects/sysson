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
import scala.concurrent.stm.{TMap, TxnExecutor, Txn, TxnLocal, Ref}
import de.sciss.lucre.{synth, stm}
import scala.concurrent.{Await, Promise, Future, ExecutionContext, future, blocking}
import de.sciss.synth.proc.{Scan, Artifact, Grapheme, Attribute, SynthGraphs, AuralPresentation, Transport, ProcGroup, Proc}
import de.sciss.lucre.synth.expr.{Longs, DoubleVec, Doubles, SpanLikes}
import de.sciss.span.Span
import de.sciss.lucre
import scala.util.control.NonFatal
import de.sciss.file._
import de.sciss.synth.io.AudioFileSpec
import de.sciss.lucre.stm.TxnLike
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import at.iem.sysson.Implicits._
import de.sciss.lucre.bitemp.BiExpr

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

  // makes sure a future is only executed when the transaction succeeds
  private def txFuture[A](block: => Future[A])(implicit tx: TxnLike): Future[A] = {
    val p = Promise[A]()
    tx.afterCommit(p.completeWith(block))
    p.future
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

  private final case class GraphemeGen(key: String, scan: Boolean, data: AudioFileCache.Result)

  private final class Impl[S <: Sys[S], I <: lucre.synth.Sys[I]](aw: AuralWorkspace[S, I],
                                                                 ap: AuralPresentation[I],
                                                                 sonifH: stm.Source[S#Tx, Sonification[S]],
      procH: stm.Source[I#Tx, Proc[I]],
      transport: Transport[I, Proc[I], Transport.Proc.Update[I]])(implicit iCursor: stm.Cursor[I], iTx: S#Tx => I#Tx)
    extends AuralSonification[S] with TxnModelImpl[S#Tx, Update] {
    impl =>

    private val _state    = Ref(Stopped: Update)
    private val attrMap   = TMap.empty[Any, String]

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
      // logInfo(s"attributeKey elem '$elem'")
      attrMap.get(elem).getOrElse(sys.error(s"No key for attribute $elem"))
    }

    private def prepare()(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = iTx(tx)
      import AudioFileCache.executionContext

      val sonif = sonifH()
      val g     = sonif.patch.graph.value
      val proc  = procH()

      var aCnt  = 0

      def addAttr(elem: Any): String = {
        val key = s"sonif_$aCnt"
        aCnt   += 1
        // logInfo(s"addAttr elem '${elem.getClass}@$elem' key '$key'")
        attrMap.put(elem, key)(tx.peer)
        key
      }

      def putAttrValue(key: String, value: Double): Unit =
        proc.attributes.put(key, Attribute.Double(Doubles.newConst(value)))

      //      def putAttrValues(key: String, values: Vec[Double]): Unit =
      //        proc.attributes.put(key, Attribute.DoubleVec(DoubleVec.newConst(values)))

      // var graphemes = Map.empty[String, Future[AudioFileCache.Result]]
      var graphemes = Vec.empty[Future[GraphemeGen]]

      g.sources.foreach {
        case uv: graph.UserValue =>
          val attrKey = addAttr(uv)
          sonif.controls.get(uv.key).foreach { expr =>
            putAttrValue(attrKey, expr.value)
          }

        case dp: graph.Dim.Play =>
          val attrKey = addAttr(dp)
          val dimElem = dp.dim
          val mapKey  = dimElem.variable.name
          val source  = sonif.sources.get(mapKey).getOrElse(throw AuralSonification.MissingSource   (mapKey))
          val dimKey  = dimElem.name
          val dimName = source.dims  .get(dimKey).getOrElse(throw AuralSonification.MissingDimension(dimKey)).value
          val ds      = source.variable.source
          val dimVar  = ds.variables.find(_.name == dimName).getOrElse(throw AuralSonification.MissingSourceDimension(dimName))
          val ranges  = dimVar.shape.map(_._2)

          // val (g, fut) = aw.graphemeCache(section)
          val fut = AudioFileCache.acquire(aw.workspace, source = dimVar, section = ranges, streamDim = -1)
          // graphemes += attrKey -> fut
          graphemes :+= fut.map(data => GraphemeGen(key = attrKey, scan = true, data = data))

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
          val dimElem = dv.dim
          val mapKey  = dimElem.variable.name
          val source  = sonif.sources.get(mapKey).getOrElse(throw AuralSonification.MissingSource(mapKey))
          // source.variable

          val section: VariableSection = ???
          // val (g, fut) = aw.graphemeCache(section)
          val fut = txFuture(AudioFileCache.acquire(aw.workspace, source = source.variable, ???, streamDim = -1))(tx)
          // graphemes += attrKey -> fut
          ??? // graphemes :+= fut.map(attrKey -> _)

        case _ =>
      }

      state_=(Preparing)

      proc.graph() = SynthGraphs.newConst[I](g)

      def transpPlay()(implicit tx: S#Tx): Unit = {
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

      if (graphemes.isEmpty) transpPlay() else tx.afterCommit {
        import ExecutionContext.Implicits.global
        future {
          blocking {
            val graphMapF = Future.sequence(graphemes)
            val graphMap  = Await.result(graphMapF, Duration(10, TimeUnit.MINUTES))
            graphMap.foreach { case gen =>
              // XXX TODO: we should allow Grapheme.Elem.newConst ?
              val gv    = gen.data
              val loc   = Artifact.Location.Modifiable[I](gv.artifact.parent)
              val artif = loc.add(gv.artifact)
              val elem  = Grapheme.Elem.Audio(artif, gv.spec, Longs.newConst(gv.offset), Doubles.newConst(gv.gain))
              if (gen.scan) {
                val scan  = proc.scans.add(gen.key)
                val g     = Grapheme.Modifiable[I]
                g.add(BiExpr(Longs.newConst(0L), elem))
                scan.addSource(Scan.Link.Grapheme(g))
              } else {
                proc.attributes.put(gen.key, Attribute.AudioGrapheme(elem))
              }
            }
          }
          aw.workspace.cursor.step { implicit tx =>
            transpPlay()
          }
        }
      }
    }
  }
}

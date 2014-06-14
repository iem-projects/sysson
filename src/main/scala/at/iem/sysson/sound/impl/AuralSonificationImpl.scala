/*
 *  AuralSonificationImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound
package impl

import de.sciss.lucre.event.Sys
import de.sciss.lucre.synth.{Sys => SSys}
import at.iem.sysson.sound.AuralSonification.{Update, Playing, Stopped, Preparing}
import at.iem.sysson.impl.TxnModelImpl
import scala.concurrent.stm.{TMap, TxnExecutor, Txn, TxnLocal, Ref}
import de.sciss.lucre.{synth, stm}
import de.sciss.lucre.expr.{Long => LongEx, Double => DoubleEx, Boolean => BooleanEx, Int => IntEx}
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import scala.concurrent.{Await, Future, ExecutionContext, blocking}
import de.sciss.synth.proc._
import de.sciss.span.Span
import de.sciss.lucre
import scala.util.control.NonFatal
import de.sciss.file._
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import de.sciss.lucre.bitemp.BiExpr
import de.sciss.lucre.matrix.{Dimension, Reduce, Matrix, DataSource}
import at.iem.sysson.graph.{Elapsed, UserValue, SonificationElement}
import scala.annotation.tailrec
import at.iem.sysson.graph

object AuralSonificationImpl {
  private val _current = TxnLocal(Option.empty[AuralSonification[_, _]])

  private[sysson] def current(): AuralSonification[_, _] = {
    implicit val tx = Txn.findCurrent.getOrElse(sys.error("Called outside of transaction"))
    _current.get.getOrElse(sys.error("Called outside transport play"))
  }

  private def using[S <: Sys[S], I <: SSys[I], A](aural: AuralSonification[S, I])(block: => A)(implicit tx: S#Tx): A = {
    val old = _current.swap(Some(aural))(tx.peer)
    val res = block
    _current.set(old)(tx.peer)
    res
  }

  //  // makes sure a future is only executed when the transaction succeeds
  //  private def txFuture[A](block: => Future[A])(implicit tx: TxnLike): Future[A] = {
  //    val p = Promise[A]()
  //    tx.afterCommit(p.completeWith(block))
  //    p.future
  //  }

  def apply[S <: Sys[S], I <: synth.Sys[I]](aw: AuralWorkspace[S, I], sonification: Obj.T[S, Sonification.Elem])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): AuralSonification[S, I] = {
    val w             = aw.workspace
    implicit val itx  = w.inMemoryBridge(tx)         // why can't I just import w.inMemory !?
    val sonifH        = tx.newHandle(sonification)
    val proc          = Proc[I]
    val obj           = Obj(Proc.Elem(proc))
    val group         = ProcGroup.Modifiable[I]
    val span          = SpanLikeEx.newVar[I](SpanLikeEx.newConst(Span.from(0L)))
    group.add(span, obj)
    import w.{inMemoryCursor, inMemoryBridge}
    val transport     = Transport[I, I](group)
    val auralSys      = AudioSystem.instance.aural
    val aural         = AuralPresentation.run(transport, auralSys)
    new Impl[S, I](aw, aural, sonifH, itx.newHandle(obj), transport)
  }

  // private sealed trait State
  // private case object Stopped extends State

  private final case class GraphemeGen(key: String, scan: Boolean, data: AudioFileCache.Result) {
    override def toString = s"GraphemeGen(key = $key, scan = $scan, " +
      s"data = ['${data.artifact.name}', numChannels = ${data.numChannels}, numFrames = ${data.spec.numFrames}])"
  }

  private final class Impl[S <: Sys[S], I <: lucre.synth.Sys[I]](aw: AuralWorkspace[S, I],
                                                                 ap: AuralPresentation[I],
                                                                 sonifH: stm.Source[S#Tx, Obj.T[S, Sonification.Elem]],
      procH: stm.Source[I#Tx, Obj.T[I, Proc.Elem]],
      transport: ProcTransport[I])(implicit iCursor: stm.Cursor[I], iTx: S#Tx => I#Tx, cursor: stm.Cursor[S])
    extends AuralSonification[S, I] with TxnModelImpl[S#Tx, Update] {
    impl =>

    private val _state    = Ref(Stopped: Update)
    private val attrMap   = TMap.empty[Any, String]

    def state(implicit tx: S#Tx): Update = _state.get(tx.peer)

    def auralPresentation = ap // .group

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

      val sonif     = sonifH()
      val sonifE    = sonif.elem.peer
      val procInObj = sonifE.proc
      val g         = procInObj.elem.peer.graph.value
      val procOutObj= procH()

      var aCnt  = 0

      def mkAttrKey(): String = {
        val key = s"son$aCnt"
        aCnt   += 1
        key
      }

      def addAttr(elem: Any): String = {
        val key = mkAttrKey()
        // logInfo(s"addAttr elem '${elem.getClass}@$elem' key '$key'")
        attrMap.put(elem, key)(tx.peer)
        key
      }

      def putAttrValue(key: String, value: Double): Unit =
        procOutObj.attr.put(key, Obj(DoubleElem(DoubleEx.newConst(value))))

      //      def putAttrValues(key: String, values: Vec[Double]): Unit =
      //        proc.attr.put(key, Attribute.DoubleVec(DoubleVec.newConst(values)))

      // var graphemes = Map.empty[String, Future[AudioFileCache.Result]]
      var graphemes = Vec.empty[Future[GraphemeGen]]

      def reduceMatrix(m: Matrix[S]): (DataSource.Variable[S], Vec[Range]) = {
        val md = m.dimensions

        /* @tailrec */ def loopMatrix(m0: Matrix[S], r0: Vec[Range]): (DataSource.Variable[S], Vec[Range]) = m0 match {
          case dv : DataSource.Variable[S] => (dv, r0)
          case mv : Matrix.Var[S]          => loopMatrix(mv(), r0)
          case red: Reduce[S] =>
            val (m1, r1) = loopMatrix(red.in, r0)
            @tailrec def loopDimension(d0: Dimension.Selection[S]): Int = d0 match {
              case dv: Dimension.Selection.Var  [S] => loopDimension(dv())
              case dn: Dimension.Selection.Name [S] => val n = dn.expr.value; md.indexWhere(_.name == n)
              case di: Dimension.Selection.Index[S] => di.expr.value
            }
            val redIdx = loopDimension(red.dim)

            @tailrec def loopOp(o0: Reduce.Op[S]): Range = o0 match {
              case ov: Reduce.Op.Var  [S] => loopOp(ov())
              case oa: Reduce.Op.Apply[S] => val i = oa.index.value; i to i
              case os: Reduce.Op.Slice[S] => os.from.value to os.to.value
            }
            val redRange  = loopOp(red.op)
            val r1i       = r1(redIdx)
            val newStart  = r1i.start + redRange.start
            val newLast   = math.min(r1i.last, r1i.start + redRange.last)
            val r2i       = newStart to newLast
            val r2        = r1.updated(redIdx, r2i)
            (m1, r2)
        }

        loopMatrix(m, md.map(dv => 0 until dv.size))
      }

      // prepares dimensional values by launching their production and adding
      // the grapheme to the map at the given `attrKey`. the method returns
      // the matrix, the reduced matrix shape and the index of the particular dimension.
      def prepareDimGE(dimElem: graph.Dim, attrKey: String,
                       streaming: Boolean): (Sonification.Source[S], Vec[Range], Int) = {
        val mapKey    = dimElem.variable.name
        val source    = getSource(mapKey)
        val dimKey    = dimElem.name
        // `dimName` is the name of the dimension in the source matrix
        // which is mapped to the logical `dimKey`
        val (dimName, mdi) = dimIndex(source, dimKey)
        val m         = source.matrix
        val (dsv, rangesM) = reduceMatrix(m)
        val ds        = dsv.source
        val dimVar    = ds.variables.find(_.name == dimName).getOrElse(
          throw AuralSonification.MissingSourceDimension(mapKey, dimName)
        )
        assert(dimVar.rank == 1)
        // val ranges    = Vec(range) // dimVar.ranges // shape.map(_._2)
        val ranges    = Vec(rangesM(mdi)) // now use only the range corresponding with the dimension at index `mdi`
        val streamDim = if (streaming) 0 else -1
        val fut       = AudioFileCache.acquire(aw.workspace, source = dimVar, section = ranges, streamDim = streamDim)
        graphemes   :+= fut.map(data => GraphemeGen(key = attrKey, scan = false, data = data))
        (source, rangesM, mdi)
      }

      // produces a graph element for dimension values
      def addDimGE(elem: graph.Dim.GE, streaming: Boolean): Unit = {
        val attrKey = addAttr(elem)
        prepareDimGE(dimElem = elem.dim, attrKey = attrKey, streaming = streaming)
      }

      // returns the name of a logical dimension and its index within a source
      def dimIndex(source: Sonification.Source[S], key: String): (String, Int) = {
        val dimName   = source.dims.get(key).getOrElse(throw AuralSonification.MissingDimension(key)).value
        val m         = source.matrix
        val md        = m.dimensions
        val mdi       = md.indexWhere(_.name == dimName)
        if (mdi < 0) throw AuralSonification.MissingDimension(key)
        (dimName, mdi)
      }

      def getSource(mapKey: String): Sonification.Source[S] =
        sonifE.sources.get(mapKey).getOrElse(throw AuralSonification.MissingSource(mapKey))

      // produces a graph element for variable values
      // TODO: perhaps factor out a bit of DRY with respect to addDimGE
      // TODO: probably we'll run `reduceMatrix` multiple times with the same input.
      // therefore, should maybe cache the results?
      def addVarGE(elem: graph.Var.GE, streaming: Option[String]): Unit = {
        val attrKey   = addAttr(elem)
        val varElem   = elem.variable
        val mapKey    = varElem.name
        val source    = getSource(mapKey)
        val m         = source.matrix
        val (dsv, rangesM) = reduceMatrix(m)
        val streamDim = streaming.fold(-1)(dimIndex(source, _)._2)
        logDebugTx(s"addVarGE: section = ${rangesM.map(r => s"${r.start} to ${r.end}")}, streamDim = $streamDim")(tx)
        val fut       = AudioFileCache.acquire(aw.workspace, source = dsv, section = rangesM, streamDim = streamDim)
        graphemes   :+= fut.map(data => GraphemeGen(key = attrKey, scan = false, data = data))
      }

      g.sources.foreach {
        case uv: graph.UserValue =>
          val attrKey = addAttr(uv)
          sonifE.controls.get(uv.key).foreach { expr =>
            putAttrValue(attrKey, expr.value)
          }

        case vp: graph.Var.Play   => addVarGE(vp, streaming = Some(vp.time.dim.name))
        case vp: graph.Var.Values => addVarGE(vp, streaming = None)

        case dp: graph.Dim.Play   => addDimGE(dp, streaming = true )
        case dv: graph.Dim.Values => addDimGE(dv, streaming = false)

        case vav: graph.Var.Axis.Values =>
          // tricky business. we use the existing approach mapping between `elem` and
          // an `attrKey` string. however, we store in `attrMap` an "extended" key which
          // is a tuple of three components separated by semicolons: the first component
          // is the actual key into the proc's attribute map (to find the grapheme) - this
          // is `attrKey0` here. the second component is the size of the dimension value
          // vector (int), the thrid component is the product of the reduced matrix shape
          // after dropping the dimensions up to and including the selected dimension.
          // that is, with respect to `VariableAxesAssociations.txt`, we store
          // "<key>;<axis_size>;<div>"
          val attrKey0 = mkAttrKey()
          // scalac freaks out - wants a return type although it is _not_ a recursive call
          val tup: (Sonification.Source[S], Vec[Range], Int) =
            prepareDimGE(dimElem = vav.axis.asDim, attrKey = attrKey0, streaming = false)
          val (source, rangesM, mdi) = tup
          val (_, timeIdx) = dimIndex(source, vav.axis.variable.time.dim.name)
          val axisSize  = rangesM(mdi).size
          val div0      = rangesM.drop(mdi + 1).map(_.size.toLong).product
          val div       = if (timeIdx < mdi) div0 else div0 / rangesM(timeIdx).size
          val attrKey   = s"$attrKey0;$axisSize;$div"
          attrMap.put(vav, attrKey)(tx.peer)

        case uv: UserValue.GE =>  // already wired up

        case el: Elapsed =>
          val attrKey = addAttr(el)
          putAttrValue(attrKey, 1000) // XXX TODO

        case nyi: SonificationElement =>
          throw new NotImplementedError(nyi.toString)

        case _ =>
      }

      state_=(Preparing)

      procOutObj.elem.peer.graph() = SynthGraphs.newConst[I](g)
      val procOutAttr = procOutObj.attr
      procInObj.attr.iterator.foreach { case (key, value) =>
        // proc-in is system S, whereas out is system I...
        // there is currently no automatic copying or referencing
        // mechanism in place. For now, what we do is create
        // "shallow" copies of the following object types:
        // AudioGrapheme, Double, Int, Boolean, FadeSpec

        value match {
          case AudioGraphemeElem.Obj(objT) =>
            val expS   : Grapheme.Expr.Audio[S] = objT.elem.peer
            val artS   : Artifact[S]            = expS.artifact
            val dir                             = artS.location.directory
            val locI                            = ArtifactLocation.Modifiable[I](dir)
            val shallow: Grapheme.Value.Audio   = expS.value
            val artI   : Artifact[I]            = locI.add(shallow.artifact)
            val exp    : Grapheme.Expr.Audio[I] =
              Grapheme.Expr.Audio(artI, shallow.spec, LongEx.newConst(shallow. offset), DoubleEx.newConst(shallow. gain))
            val objI    = Obj(AudioGraphemeElem(exp))
            procOutAttr.put(key, objI)

          case DoubleElem.Obj(objT) =>
            val shallow = objT.elem.peer.value
            val objI    = Obj(DoubleElem(DoubleEx.newConst[I](shallow)))
            procOutAttr.put(key, objI)

          case IntElem.Obj(objT) =>
            val shallow = objT.elem.peer.value
            val objI    = Obj(IntElem(IntEx.newConst[I](shallow)))
            procOutAttr.put(key, objI)

          case BooleanElem.Obj(objT) =>
            val shallow = objT.elem.peer.value
            val objI    = Obj(BooleanElem(BooleanEx.newConst[I](shallow)))
            procOutAttr.put(key, objI)

          case FadeSpec.Elem.Obj(objT) =>
            val shallow = objT.elem.peer.value
            val objI    = Obj(FadeSpec.Elem(FadeSpec.Expr.newConst[I](shallow)))
            procOutAttr.put(key, objI)

          case _ =>
        }
      }

      // now either play directly or prepare graphemes first

      if (graphemes.isEmpty) transportPlay() else tx.afterCommit {
        import ExecutionContext.Implicits.global
        logDebug(s"Producing ${graphemes.size} graphemes...")
        Future {
          val graphMap = blocking {
            val graphMapF = Future.sequence(graphemes)
            Await.result(graphMapF, Duration(10, TimeUnit.MINUTES))
          }
          logDebug("Finished grapheme production")
          produceGraphemesAndPlay(graphMap)
        }
      }
    }

    // private def cursor: stm.Cursor[S] = aw.workspace.cursor

    // reset and start transport, after the Proc was fully configured
    def transportPlay()(implicit tx: S#Tx): Unit = {
      logDebugTx("Transport play")(tx)
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

    private def produceGraphemesAndPlay(graphMap: Vec[GraphemeGen]): Unit = cursor.step { implicit tx =>
      implicit val itx: I#Tx = iTx(tx)
      val proc = procH()
      graphMap.foreach { gen =>
        logDebug(s"step $gen")
        // XXX TODO: we should allow Grapheme.Elem.newConst ?
        val gv    = gen.data
        val loc   = ArtifactLocation.Modifiable[I](gv.artifact.parent)
        val artif = loc.add(gv.artifact)
        val elem  = Grapheme.Expr.Audio(artif, gv.spec, LongEx.newConst(gv.offset), DoubleEx.newConst(gv.gain))
        if (gen.scan) {
          val scan  = proc.elem.peer.scans.add(gen.key)
          val g     = Grapheme.Modifiable[I]
          g.add(BiExpr(LongEx.newConst(0L), elem))
          scan.addSource(Scan.Link.Grapheme(g))
        } else {
          proc.attr.put(gen.key, Obj(AudioGraphemeElem(elem)))
        }
      }
      transportPlay()
    }
  }
}

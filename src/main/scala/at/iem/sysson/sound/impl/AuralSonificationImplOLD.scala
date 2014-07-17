/*
 *  AuralSonificationImplOLD.scala
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
import de.sciss.lucre.synth.expr.DoubleVec
import de.sciss.lucre.synth.{Sys => SSys, Server, Txn}
import at.iem.sysson.sound.AuralSonificationOLD.{Update, Playing, Stopped, Preparing}
import at.iem.sysson.impl.TxnModelImpl
import de.sciss.synth.proc.{SynthGraphs, FadeSpec, Scan, BooleanElem, ArtifactLocation, Artifact, Grapheme, AudioGraphemeElem, AuralSystem, ProcTransport, IntElem, DoubleVecElem, DoubleElem, TransportOLD => Transport, ProcGroup, Obj, Proc}
import scala.concurrent.stm.{TMap, TxnExecutor, Txn => ScalaTxn, TxnLocal, Ref}
import de.sciss.lucre.{synth, stm}
import de.sciss.lucre.expr.{Long => LongEx, Double => DoubleEx, Boolean => BooleanEx, Int => IntEx}
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import scala.concurrent.{Await, Future, ExecutionContext, blocking}
import de.sciss.span.Span
import de.sciss.{osc, lucre}
import scala.util.control.NonFatal
import de.sciss.file._
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import de.sciss.lucre.bitemp.BiExpr
import de.sciss.lucre.matrix.{Dimension, Reduce, Matrix, DataSource}
import at.iem.sysson.graph.SonificationElement
import scala.annotation.tailrec
import de.sciss.synth.message
import scala.collection.breakOut

object AuralSonificationImplOLD {
  private val _current = TxnLocal(Option.empty[AuralSonificationOLD[_, _]])

  private[sysson] def current(): AuralSonificationOLD[_, _] = {
    implicit val tx = ScalaTxn.findCurrent.getOrElse(sys.error("Called outside of transaction"))
    _current.get.getOrElse(sys.error("Called outside transport play"))
  }

  private def using[S <: Sys[S], I <: SSys[I], A](aural: AuralSonificationOLD[S, I])(block: => A)(implicit tx: S#Tx): A = {
    val old = _current.swap(Some(aural))(tx.peer)
    val res = block
    _current.set(old)(tx.peer)
    res
  }

  def apply[S <: Sys[S], I <: synth.Sys[I]](aw: AuralWorkspace[S, I], sonification: Obj.T[S, Sonification.Elem])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): AuralSonificationOLD[S, I] = {
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
    // val auralSys      = AudioSystem.instance.aural
    // val aural: AuralPresentation[I] = ??? //         = AuralPresentation.run(transport, auralSys)
    val res           = new Impl[S, I](aw, /* aural, */ sonifH, itx.newHandle(obj), transport)
    res.init()
    // auralSys.addClient(res)
    // auralSys.serverOption.foreach(s => res.auralStarted(s))
    res
  }

  private final case class GraphemeGen(key: String, scan: Boolean, data: AudioFileCache.Result) {
    override def toString = s"GraphemeGen(key = $key, scan = $scan, " +
      s"data = ['${data.artifact.name}', numChannels = ${data.numChannels}, numFrames = ${data.spec.numFrames}])"
  }

  private final class Impl[S <: Sys[S], I <: lucre.synth.Sys[I]](aw: AuralWorkspace[S, I],
                                                                 // ap: AuralPresentation[I],
                                                                 sonifH: stm.Source[S#Tx, Obj.T[S, Sonification.Elem]],
      procH: stm.Source[I#Tx, Obj.T[I, Proc.Elem]],
      transport: ProcTransport[I])(implicit iCursor: stm.Cursor[I], iTx: S#Tx => I#Tx, cursor: stm.Cursor[S])
    extends AuralSonificationOLD[S, I] with TxnModelImpl[S#Tx, Update] with AuralSystem.Client {
    impl =>

    private val _state    = Ref(Stopped: Update)
    private val attrMap   = TMap.empty[Any, String]

    def state(implicit tx: S#Tx): Update = _state.get(tx.peer)

    // def auralPresentation = ap // .group

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

    def attributeKey(elem: Any): String = {
      logDebug(s"attributeKey elem '$elem'")
      TxnExecutor.defaultAtomic { implicit itx =>
        attrMap.get(elem).getOrElse(sys.error(s"No key for attribute $elem"))
      }
    }

    // XXX TODO: one problem with this approach is that
    // the aural-sonification is kept running for the entire duration of
    // the session, and never disposed. thus, we may end up having quite
    // a lot of responders registered, even if they will not match any
    // messages.
    //
    // On the up-side, we can use this approach to manage online-buffers.
    private val elapsedResp = Ref(Option.empty[message.Responder])

    private val elapsedMap  = TMap.empty[Int, graph.Elapsed]

    def auralStarted(s: Server)(implicit tx: Txn): Unit = {
      // ---- first thing, install the responder to handle elapsed notifications ----
      installResponder(s)
      // ---- second thing, look for random access buffers, and start loading them ----
      loadBuffers(s)
    }

    def init()(implicit tx: S#Tx): Unit = {
      // the random access buffers feature is
      // not yet implemented, only the tentative layout
      // exists:
      // - trace the appearance of attr map entries connected
      //   to LoadBuffer elements in the graph sources
      // - these pairs are communicated to the AuralWorkspace
      //   which will keep a usage count for the graphemes
      // - when the server is ready, the workspace will
      //   sequentially load the registered graphemes,
      //   producing a Processor or Future.
      // - the sonif will query these, and add them to the
      //   stuff to wait for during preparation.
      // - it must be decided how to handle the case that
      //   the graph function changes during playback. because
      //   that is the moment that buffers might be registered
      //   and unregistered, and that must not interfere with
      //   ongoing use. the straight forward answer is to stop
      //   the transport when that happens.

      // scanForBuffers()
    }

    private def scanForBuffers()(implicit tx: S#Tx): Unit = {
      val procObj   = sonifH().elem.peer.proc // yes, I know... we ought to simplify this
      val proc      = procObj.elem.peer
      val procAttr  = procObj.attr
      val g         = proc.graph.value
      val map: Map[String, Grapheme.Value.Audio] = g.sources.flatMap {
        case elem: graph.LoadBuffer =>
          val key = elem.key
          // println(s"LoadBuffer($key)")
          procAttr.expr[Grapheme.Value.Audio](key).map { expr =>
            val audio = expr.value
            // val artif = audio.artifact
             // println(s"---> $artif")
            (key, audio)
          }
        case _ => None
      } (breakOut)

      map.valuesIterator.foreach { audio =>
        aw.addBuffer(audio)
      }
    }

    private def installResponder(s: Server)(implicit tx: Txn): Unit = {
      val resp = message.Responder(s.peer) {
        case osc.Message("/$elpsd", _, reportID: Int, ratio: Float, value: Float) =>
          // println(s"elapsed: $reportID, $ratio")
          // use `.single.get` so we stay lightweight if the id is not associated with this sonif.
          elapsedMap.single.get(reportID).foreach { elem =>
            cursor.step { implicit tx =>
              dispatch(AuralSonificationOLD.Elapsed(elem.in.dim.name, ratio = ratio, value = value))
              // val debug = elem.terminate || ratio >= 1
              // if (debug) println(s"terminate? ${elem.terminate}; ratio? $ratio")
              if (ratio >= 1 && elem.terminate) stop()
            }
          }
      }
      elapsedResp.set(Some(resp))(tx.peer)
      tx.afterCommit(resp.add())
    }

    private def loadBuffers(s: Server)(implicit tx: Txn): Unit = {
    }

    def auralStopped()(implicit tx: Txn): Unit = {
      elapsedResp.swap(None)(tx.peer).foreach(resp => tx.afterCommit(resp.remove()))
    }

    @inline private def clearTMap[A, B](m: TMap[A, B])(implicit tx: S#Tx): Unit =
      m.retain((_, _) => false)(tx.peer)  // aka .clear()

    private def prepare()(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = iTx(tx)
      import AudioFileCache.executionContext

      clearTMap(attrMap)
      clearTMap(elapsedMap)

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
        logDebug(s"addAttr elem '${elem.getClass}@$elem' key '$key'")
        attrMap.put(elem, key)(tx.peer)
        key
      }

      def isNewAttr(elem: Any): Boolean = !attrMap.contains(elem)(tx.peer)

      def putAttrValue(key: String, value: Double): Unit =
        procOutObj.attr.put(key, Obj(DoubleElem(DoubleEx.newConst(value))))

      def putAttrValueI(key: String, value: Int): Unit =
        procOutObj.attr.put(key, Obj(IntElem(IntEx.newConst(value))))

      def putAttrValues(key: String, values: Vec[Double]): Unit =
        procOutObj.attr.put(key, Obj(DoubleVecElem(DoubleVec.newConst(values))))

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
        val (source, dsv, dimName, rangesM, mdi) = findDim(dimElem)
        val ds        = dsv.source
        val mapKey    = dimElem.variable.name
        val dimVar    = ds.variables.find(_.name == dimName).getOrElse(
          throw AuralSonificationOLD.MissingSourceDimension(mapKey, dimName)
        )
        assert(dimVar.rank == 1)
        val ranges    = Vec(rangesM(mdi)) // now use only the range corresponding with the dimension at index `mdi`
        val streamDim = if (streaming) 0 else -1
        val fut       = AudioFileCache.acquire(aw.workspace, source = dimVar, section = ranges, streamDim = streamDim)
        graphemes   :+= fut.map(data => GraphemeGen(key = attrKey, scan = false, data = data))
        (source, rangesM, mdi)
      }

      // finds a mapped dimension. returns a tuple consisting of
      // (sonification source to which the dim is mapped,
      //  dimension's data source,
      //  dimension-name within the data source,
      //  reduced matrix of the source,
      //  index of the dimension within the reduced matrix)
      def findDim(dimElem: graph.Dim): (Sonification.Source[S], DataSource.Variable[S], String, Vec[Range], Int) = {
        val mapKey    = dimElem.variable.name
        val source    = getSource(mapKey)
        val dimKey    = dimElem.name
        // `dimName` is the name of the dimension in the source matrix
        // which is mapped to the logical `dimKey`
        val (dimName, mdi) = dimIndex(source, dimKey)
        val m         = source.matrix
        val (dsv, rangesM) = reduceMatrix(m)
        (source, dsv, dimName, rangesM, mdi)
      }

      // produces a graph element for dimension values
      def addDimGE(elem: graph.Dim.GE, streaming: Boolean): Unit = if (isNewAttr(elem)) {
        val attrKey = addAttr(elem)
        prepareDimGE(dimElem = elem.dim, attrKey = attrKey, streaming = streaming)
      }

      // returns the name of a logical dimension and its index within a source
      def dimIndex(source: Sonification.Source[S], key: String): (String, Int) = {
        val dimName   = source.dims.get(key).getOrElse(throw AuralSonificationOLD.MissingDimension(key)).value
        val m         = source.matrix
        val md        = m.dimensions
        val mdi       = md.indexWhere(_.name == dimName)
        if (mdi < 0) throw AuralSonificationOLD.MissingDimension(key)
        (dimName, mdi)
      }

      def getSource(mapKey: String): Sonification.Source[S] =
        sonifE.sources.get(mapKey).getOrElse(throw AuralSonificationOLD.MissingSource(mapKey))

      // produces a graph element for variable values
      // TODO: perhaps factor out a bit of DRY with respect to addDimGE
      // TODO: probably we'll run `reduceMatrix` multiple times with the same input.
      // therefore, should maybe cache the results?
      def addVarGE(elem: graph.Var.GE, streaming: Option[String]): Unit = if (isNewAttr(elem)) {
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
        case elem: graph.UserValue =>
          if (isNewAttr(elem)) {
            val attrKey = addAttr(elem)
            sonifE.controls.get(elem.key).foreach { expr =>
              putAttrValue(attrKey, expr.value)
            }
          }

        case elem: graph.Var.Play       => addVarGE(elem, streaming = Some(elem.time.dim.name))
        case elem: graph.Var.Values     => addVarGE(elem, streaming = None)

        case elem: graph.Dim.Play       => addDimGE(elem, streaming = true )
        case elem: graph.Dim.Values     => addDimGE(elem, streaming = false)
        case elem: graph.Dim.IndexRange =>
          if (isNewAttr(elem)) {
            val attrKey = addAttr(elem)
            val (_, _, _, rangesM, mdi) = findDim(elem.dim)
            val range   = rangesM(mdi)
            val values  = Vec[Double](range.head, range.last + range.step /* aka terminalElement */)
            putAttrValues(attrKey, values)
          }

        case elem: graph.Var.Axis.Values =>
          // tricky business. we use the existing approach mapping between `elem` and
          // an `attrKey` string. however, we store in `attrMap` an "extended" key which
          // is a tuple of three components separated by semicolons: the first component
          // is the actual key into the proc's attribute map (to find the grapheme) - this
          // is `attrKey0` here. the second component is the size of the dimension value
          // vector (int), the third component is the product of the reduced matrix shape
          // after dropping the dimensions up to and including the selected dimension.
          // that is, with respect to `VariableAxesAssociations.txt`, we store
          // "<key>;<axis_size>;<div>"
          if (isNewAttr(elem)) {
            val attrKey0      = mkAttrKey()
            // scalac freaks out - wants a return type although it is _not_ a recursive call
            val tup: (Sonification.Source[S], Vec[Range], Int) =
              prepareDimGE(dimElem = elem.axis.asDim, attrKey = attrKey0, streaming = false)
            val (source, rangesM, mdi) = tup
            val (_, timeIdx)  = dimIndex(source, elem.axis.variable.time.dim.name)
            val axisSize      = rangesM(mdi).size
            val div0          = rangesM.drop(mdi + 1).map(_.size.toLong).product
            val div           = if (timeIdx < mdi) div0 else div0 / rangesM(timeIdx).size
            val attrKey       = s"$attrKey0;$axisSize;$div"
            attrMap.put(elem, attrKey)(tx.peer)
          }

        case elem: graph.UserValue.GE =>  // already wired up

        case elem: graph.Elapsed =>
          if (isNewAttr(elem)) {
            // how this works: the GE uses a normal attribute/control
            // to receive the actual report-ID which is used for SendReply.
            // For simplicity, the aural-workspace provides an incremental
            // ID generator (similar to node IDs). That way, there will
            // be no conflicts between multiple aural-sonifs.
            // The aural-sonif on the other hand, must then be able
            // to trace that ID back to the dimension. It will then
            // dispatch an update for the visual views to watch.
            val attrKey   = addAttr(elem)
            val reportID  = aw.nextID()
            putAttrValueI(attrKey, reportID)
            elapsedMap.put(reportID, elem)(tx.peer)
          }

        case elem: SonificationElement =>
          throw new NotImplementedError(elem.toString)

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
          val g     = Grapheme.Modifiable[I](gv.numChannels)
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

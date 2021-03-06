/*
 *  AuralSonificationImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
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

import at.iem.sysson.graph.UserValue
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.DoubleObj
import de.sciss.lucre.matrix.{DataSource, Matrix => LMatrix}
import de.sciss.lucre.stm.{Disposable, Obj, Source, Sys}
import de.sciss.lucre.synth.{NodeRef, Sys => SSys}
import de.sciss.lucre.{stm, event => evt}
import de.sciss.synth.proc.impl.{AsyncProcBuilder, AuralProcImpl}
import de.sciss.synth.proc.{AuralContext, AuralObj, Gen, Runner, TimeRef, UGenGraphBuilder => UGB}
import de.sciss.{equal, numbers}
import ucar.nc2.time.{CalendarDateFormatter, CalendarPeriod}

import scala.concurrent.stm.{InTxn, TMap, TxnLocal}
import scala.util.control.NonFatal

object AuralSonificationImpl {
  private[this] val auralSonifLoc = TxnLocal[Sonification[_]]()

  def use[S <: Sys[S], A](sonification: Sonification[S])(body: => A)(implicit tx: S#Tx): A = {
    val old = auralSonifLoc.swap(sonification)(tx.peer)
    try {
      body
    } finally {
      auralSonifLoc.set(old)(tx.peer)
    }
  }

  def find[S <: Sys[S]]()(implicit tx: S#Tx): Option[Sonification[S]] = {
    Option(auralSonifLoc.get(tx.peer).asInstanceOf[Sonification[S]])
  }

  def apply[S <: SSys[S]](obj: Sonification[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralSonification[S] = {
    logDebugTx(s"AuralSonification($obj)")
    val objH      = tx.newHandle(obj)
    val res       = new Impl[S]
    val procView  = new ProcImpl(res, context)
    // order is crucial:
    res.init(objH, procView)
    procView.initSonif(obj)
    res
  }

  private def findSource[S <: SSys[S]](obj: Sonification[S], variable: graph.MatrixLike)
                                     (implicit tx: S#Tx): Sonification.Source[S] = {
    val varKey  = variable.name
    val source  = obj.sources.get(varKey).getOrElse(sys.error(s"Missing source for key '$varKey'"))
    source
  }

  private def findDimIndex[S <: SSys[S]](source: Sonification.Source[S], dimElem: graph.Dim)
                                       (implicit tx: S#Tx): Int = {
    val dimKey  = dimElem.name
    val dimName = source.dims.get(dimKey).getOrElse(sys.error(s"Missing dimension mapping for key '$dimKey'")).value
    val full    = source.matrix
    import equal.Implicits._
    val dimIdx  = full.dimensions.indexWhere(_.name === dimName)
    if (dimIdx < 0) sys.error(s"Dimension '$dimName' not in matrix")
    dimIdx
  }

  private final class ProcImpl[S <: SSys[S]](sonifData: Impl[S], context0: AuralContext[S])
    extends AuralProcImpl.Impl[S]()(context0) {

    private val ctlMap = TMap.empty[String, Disposable[S#Tx]]

    import sonifData.sonification

    def initSonif(obj: Sonification[S])(implicit tx: S#Tx): this.type = {
      val controls = obj.controls
      val obsCtl = controls.changed.react { implicit tx => upd => upd.changes.foreach {
        case evt.Map.Added   (key, value)       => ctlAdded  (key, value)
        case evt.Map.Removed (key, value)       => ctlRemoved(key, value)
        case evt.Map.Replaced(key, _ /* before */, now) => ctlChange (key, Some(now.value))
      }}
      addObserver(obsCtl)
      controls.iterator.foreach { case (key, value) =>
        mkCtlObserver(key, value)
      }
      init(obj.proc)
    }

    // ---- attr events ----

    override protected def invokeRetry(state: UGB.Incomplete[S])(implicit tx: S#Tx): UGB.State[S] =
      use(sonification)(super.invokeRetry(state))

    private def ctlAdded(key: String, value: DoubleObj[S])(implicit tx: S#Tx): Unit = {
      mkCtlObserver(key, value)
      ctlChange(key, Some(value.value))
    }

    private def mkCtlObserver(key: String, value: DoubleObj[S])(implicit tx: S#Tx): Unit = {
      // XXX TODO -- should have smarter observers that pull the changed
      // values directly
      val obs = value.changed.react { implicit tx => upd => ctlChange(key, Some(upd.now)) }
      ctlMap.put(key, obs)(tx.peer)
    }

    private def ctlRemoved(key: String, value: DoubleObj[S])(implicit tx: S#Tx): Unit = {
      implicit val ptx: InTxn = tx.peer
      ctlMap.remove(key).foreach(_.dispose())
      ctlChange(key, None)
    }

    private def ctlChange(key: String, valueOpt: Option[Double])(implicit tx: S#Tx): Unit = nodeOption match {
      case Some(n: NodeRef.Full[S]) =>
        val valueOpt1: Option[Double] = valueOpt.orElse {
          val m = buildState.acceptedInputs.getOrElse(UserValue.Key(key), Map.empty)
          m.collectFirst {
            case (UserValue(_, default), _) => default
          }
        }

        valueOpt1 /* sonifCached().controls.get(key) */ .foreach { value =>
          ctlNodeSet1(n, key, value)
        }
      case _ =>
    }

    private def ctlNodeSet1(n: NodeRef.Full[S], key: String, expr: DoubleObj[S])
                            (implicit tx: S#Tx): Unit = {
      val ctlName = UserValue.controlName(key)
      n.addControl(ctlName -> expr.value)
    }

    private def oldMatrixSpecs(req: UGB.Input, st: UGB.IO[S]): Vec[MatrixPrepare.Spec] =
      st.acceptedInputs.getOrElse(req.key, Map.empty).collectFirst {
        case (_, mv: MatrixPrepare.Value) => mv.specs
        //        case (_, other) =>
        //          logDebug(s"For key '${req.key}' found something other than MatrixPrepare.Value: $other")
        //          Vector.empty
      }.getOrElse(Vector.empty)

    private def addSpec(req: UGB.Input, st: UGB.IO[S], spec: MatrixPrepare.Spec): MatrixPrepare.Value =
      MatrixPrepare.Value(oldMatrixSpecs(req, st) :+ spec)

    private def findMatrix(variable: graph.MatrixLike)(implicit tx: S#Tx): LMatrix[S] = {
      val sonif     = sonification
      val varKey    = variable.name
      val directOpt = sonif.sources.get(varKey)
      directOpt.fold[LMatrix[S]] {
        def resolveValue(v: Obj[S]): LMatrix[S] = v match {
          case mat: LMatrix[S] => mat
          case a: Gen[S] =>
            val genView   = mkGenView(a, varKey)
            val tryValue  = genView.value.getOrElse(throw UGB.MissingIn(UGB.AttributeKey(varKey)))
            val newValue  = tryValue.get
            resolveValue(newValue)

          case a => throw new IllegalStateException(s"Unsupported matrix input $a")
        }
        val proc    = procCached()
        val attrVal = proc.attr.get(varKey).getOrElse(sys.error(s"Missing source for key '$varKey'"))
        resolveValue(attrVal)

      } { source =>
        source.matrix
      }
    }

    private def findMatrixAndDimIndex(variable: graph.MatrixLike,
                                      dimElem: graph.Dim)(implicit tx: S#Tx): (LMatrix[S], Int) = {
      val sonif     = sonification
      val varKey    = variable.name
      val varKey2   = dimElem.variable.name
      val direct    = varKey == varKey2
      val directOpt = sonif.sources.get(varKey)
      val sourceOpt = directOpt.orElse {
        // if the variable differs from the dimension variable,
        // we assume that the program wants to find the equivalent
        // dimension in it, referring to a matrix that is stored
        // in the proc's attribute map.
        if (direct) None else sonif.sources.get(varKey2)
      }
      val source    = sourceOpt.getOrElse(sys.error(s"Missing source for key '$varKey2'"))
      val dimIdx    = findDimIndex(source, dimElem)
      val sourceMat = source.matrix

      if (direct) {
        sourceMat -> dimIdx
      } else {
        val dimName = sourceMat.dimensions.apply(dimIdx).name

        def resolveValue(v: Obj[S]): (LMatrix[S], Int) = v match {
          case mat: LMatrix[S] =>
            val dimIdx = mat.dimensions.indexWhere(_.name == dimName)
            if (dimIdx < 0) sys.error(s"Cannot find dimension '${dimElem.name}' -> '$dimName' in ${mat.name}")
            mat -> dimIdx

          case a: Gen[S] =>
            val genView   = mkGenView(a, varKey)
            val tryValue  = genView.value.getOrElse(throw UGB.MissingIn(UGB.AttributeKey(varKey)))
            val newValue  = tryValue.get
            resolveValue(newValue)

          case a => throw new IllegalStateException(s"Unsupported matrix input $a")
        }

        val proc    = procCached()
        val attrVal = proc.attr.get(varKey).getOrElse(sys.error(s"Missing source for key '$varKey'"))
        resolveValue(attrVal)
      }
    }

    override def requestInput[Res](req: UGB.Input { type Value = Res }, st: UGB.Requester[S])
                                  (implicit tx: S#Tx): Res = req match {
      case _: graph.UserValue | _: graph.Dim.Size | _: graph.Var.Size | _: graph.Elapsed | _: graph.Calendar.GE =>
        UGB.Unit

      case dp: graph.Dim.Play  =>
        val numCh     = 1 // a streaming dimension is always monophonic
        val newSpec   = MatrixPrepare.Spec(numChannels = numCh, elem = dp, streamDim = 0)
        addSpec(req, st, newSpec)

      case dv: graph.Dim.Values =>
//        val sonif     = sonification
        val dimElem   = dv.dim
//        val source    = findSource  (sonif , dv.variable)
//        val dimIdx    = findDimIndex(source, dimElem)
        val (mat, dimIdx) = findMatrixAndDimIndex(dv.variable, dimElem)
        val numCh     = mat.shape.apply(dimIdx)
        val newSpec   = MatrixPrepare.Spec(numChannels = numCh, elem = dv, streamDim = -1)
        addSpec(req, st, newSpec)

      case vp: graph.Var.Play =>
        val dimElem   = vp.time.dim
        val (mat, dimIdx) = findMatrixAndDimIndex(vp.variable, dimElem)
        val shape     = mat.shape
        val numCh     = ((1L /: shape)(_ * _) / shape(dimIdx)).toInt
        logDebug(s"graph.Var.Play - numChannels = $numCh")
        val newSpec   = MatrixPrepare.Spec(numChannels = numCh, elem = vp, streamDim = dimIdx)
        addSpec(req, st, newSpec)

      case vv: graph.Var.Values =>
        val sonif     = sonification
        val source    = findSource(sonif, vv.variable)
        val numChL    = source.matrix.size
        if (numChL > 0xFFFF) sys.error(s"$vv - matrix too large ($numChL cells)")
        val numCh     = numChL.toInt
        val newSpec   = MatrixPrepare.Spec(numChannels = numCh, elem = vv, streamDim = -1)
        addSpec(req, st, newSpec)

      case av: graph.Var.Axis.Values =>
        val timeDim           = av.axis.variable.time.dim
        val axisDim           = av.axis.asDim // Dim(timeDim.variable, av.axis.dim)
        val (mat, streamIdx)  = findMatrixAndDimIndex(av.variable, timeDim)
        val (_  , axisIdx  )  = findMatrixAndDimIndex(av.variable, axisDim)
        MatrixPrepare.ShapeAndIndex(shape = mat.shape, streamIndex = streamIdx, axisIndex = axisIdx)

      case _ =>
        val sonif     = sonification
        auralSonifLoc.set(sonif)(tx.peer) // XXX TODO -- dirty hack, store txn-local here
        super.requestInput(req, st)
    }

    override def dispose()(implicit tx: S#Tx): Unit = {
      super.dispose()
      implicit val ptx = tx.peer
      ctlMap.foreach(_._2.dispose())
      ctlMap.clear()
    }

    // A nasty override to be able to catch `IndexOutOfBoundsException`
    // thrown when `UGenGraph` expansion exceed maximum number of wire buffers
    override protected def launch(ugen: UGB.Complete[S], timeRef: TimeRef)(implicit tx: S#Tx): Unit = try {
      super.launch(ugen, timeRef)
    } catch {
      case NonFatal(e) =>
        // XXX TODO -- yes, really ugly to catch that exception
        // within a transaction. but in the no-scans version
        // of SP we'll find a cleaner solution
        sonifData.status.update(AuralSonification.PlayFailed(e))
    }

    override protected def buildSyncInput(b: NodeRef.Full[S], timeRef: TimeRef, keyW: UGB.Key, value: UGB.Value)
                                         (implicit tx: S#Tx): Unit = keyW match {
      case UserValue.Key(key) =>
        val sonif = sonification // .elem.peer
        sonif.controls.get(key).foreach { expr =>
          val ctlName = UserValue.controlName(key)
          b.addControl(ctlName -> expr.value)
          // b.setMap += ctlName -> expr.value
        }

      case sz: graph.Dim.Size =>
        val sonif     = sonification
        val dimElem   = sz.dim
        val source    = findSource  (sonif , dimElem.variable)
        val dimIdx    = findDimIndex(source, dimElem)
        val value     = source.matrix.shape.apply(dimIdx)
        val ctlName   = sz.ctlName
        b.addControl(ctlName -> value)
//        b.setMap += ctlName -> value

      case sz: graph.Var.Size =>
        val sonif     = sonification
        val source    = findSource  (sonif , sz.variable)
        val value     = source.matrix.size
        val ctlName   = sz.ctlName
        b.addControl(ctlName -> value.toFloat)
//        b.setMap += ctlName -> value.toFloat

      case el: graph.Elapsed =>
        val resp = new graph.Elapsed.Responder(key = el.in.dim, status = sonifData.status, synth = b.node)
        // println("ADD ELAPSED RESPONDER USER")
//        b.users ::= resp
        b.addUser(resp)

      case cal: graph.Calendar.GE =>
        val sonif       = sonification
        val dimElem     = cal.time.dim
        val source      = findSource  (sonif , dimElem.variable)
        val dimIdx      = findDimIndex(source, dimElem)
        val dimMat      = source.matrix.dimensions.apply(dimIdx)
        val units       = dimMat.units

        val daysPerYear = 365.2422  // average according to NASA
        val secPerDay   = 86400
        val ctlName     = cal.ctlName

        val (calDate, factor) = if (units.startsWith("days since")) {
          val date  = CalendarDateFormatter.isoStringToCalendarDate(null, units.substring(11))
          (date, 1)
        } else if (units.startsWith("hours since")) {
          val date = CalendarDateFormatter.isoStringToCalendarDate(null, units.substring(12))
          (date, 24)

        } else if (units.startsWith("seconds since")) {
          val date = CalendarDateFormatter.isoStringToCalendarDate(null, units.substring(14))
          (date, 24 * 60 * 60)

        } else {
          val message = if (units.isEmpty) "without units" else s"from units '$units'"
          sys.error(s"Cannot deduce calendar $message")
        }

        val dateT   = calDate.truncate   (CalendarPeriod.Field.Year)
        val offYear = dateT.getFieldValue(CalendarPeriod.Field.Year)
        val diffMs  = calDate.getDifferenceInMsecs(dateT)
        val diffSec = diffMs * 0.001
        val diffDay = (diffSec / secPerDay) + (366 - daysPerYear)

        val (_add, _mul2) = cal match {
          case _: graph.Calendar.Month =>
            val add     = diffDay
            val mul2    = 12.0 / daysPerYear
            (add, mul2)

          case _: graph.Calendar.Year  =>
            val offDays = offYear * daysPerYear
            val add     = diffDay + offDays
            val mul2    = 1.0 / daysPerYear
            (add, mul2)
        }
        val mul1    = 1.0 / factor
        b.addControl(ctlName -> Vector(mul1.toFloat, _add.toFloat, _mul2.toFloat))

      case _: graph.Var.Axis.Key =>   // nothing to be done

      case _ => super.buildSyncInput(b, timeRef, keyW, value)
    }

    override protected def buildAsyncInput(b: AsyncProcBuilder[S], keyW: UGB.Key, value: UGB.Value)
                                          (implicit tx: S#Tx): Unit = value match {
      case MatrixPrepare.Value(specs) =>
        specs.zipWithIndex.foreach { case (spec, idx) =>
          addMatrixStream(b, spec = spec, idx = idx)
        }

      case _ => super.buildAsyncInput(b, keyW, value)
    }

    private def addMatrixStream(b: AsyncProcBuilder[S], spec: MatrixPrepare.Spec, idx: Int)
                               (implicit tx: S#Tx): Unit = {
      // note: info-only graph elems not yet supported (or existent)
      import context.universe.workspace
      import spec.{elem, streamDim}
      implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]

      //      val matrix  = if (isDim) {
      //        val dimIdx  = findDimIndex(source, elem)
      //        full.getDimensionKey(dimIdx, useChannels = streamDim < 0)
      //      } else {
      //        full.getKey(streamDim)
      //      }
      val matrix: LMatrix.ReaderFactory[S] = elem match {
        case dimGE: MatrixPrepare.DimGE =>
          val source  = findSource(sonification, elem.variable)
          val full    = source.matrix
          val dimIdx = findDimIndex(source, dimGE.key)
          full.prepareDimensionReader(dimIdx, useChannels = streamDim < 0)

        case _ =>
          val full    = findMatrix(elem.variable)
          full.prepareReader(streamDim)
      }

      // val ctlName     = de.sciss.synth.proc.graph.stream.controlName(key, idx)
      //  val ctlName     = proc.graph.impl.Stream.controlName(key, idx)
      val bufSize     = /* if (spec.isEmpty) server.config.blockSize else */ {
        val maxFreq   = elem match {
          case st: MatrixPrepare.PlayGE => if (st.maxFreq <= 0.0) 1.0 else st.maxFreq
          case _ => 1.0
        }
        val maxSpeed  = maxFreq / server.sampleRate
        val bufDur    = 1.5 * maxSpeed
        val minSz     = (2 * server.config.blockSize * math.max(1.0, maxSpeed)).toInt
        val bestSz    = math.max(minSz, (bufDur * server.sampleRate).toInt)
        import numbers.Implicits._
        val bestSzHi  = bestSz.nextPowerOfTwo
        val bestSzLo  = bestSzHi >> 1
        if (bestSzHi.toDouble/bestSz < bestSz.toDouble/bestSzLo) bestSzHi else bestSzLo
      }

      val key = MatrixPrepare.mkKey(elem) // mkKeyOLD(dimElem, isDim = isDim) // graph.Dim.key(dimElem)
      val cfg = MatrixPrepare.Config(matrix = matrix, server = server, key = key, index = idx, bufSize = bufSize)
      import context.universe.genContext
      val res = MatrixPrepare(cfg)
      b.resources ::= res
    }
  }

  private final class Impl[S <: SSys[S]]
    extends AuralSonification[S] with ObservableImpl[S, Runner.State] {

    def tpe: Obj.Type = Sonification

    private var procViewL: Disposable[S#Tx] = _

    private val sonifLoc = TxnLocal[Sonification[S]]() // cache-only purpose

    private var _obj: stm.Source[S#Tx, Sonification[S]] = _
    private var _procView: AuralObj[S] = _

    def objH: Source[S#Tx, Obj[S]] = _obj

    def obj: stm.Source[S#Tx, Sonification[S]] = _obj

    object status
      extends ObservableImpl[S, AuralSonification.Update /* [S] */]
      with stm.Sink[S#Tx, AuralSonification.Update /* [S] */] {

      def update(u: AuralSonification.Update /* [S] */)(implicit tx: S#Tx): Unit = fire(u)
    }

    def init(obj: stm.Source[S#Tx, Sonification[S]], procView: AuralObj[S])(implicit tx: S#Tx): this.type = {
      _obj      = obj
      _procView = procView
      procViewL = procView.react { implicit tx => upd =>
        fire(upd) // just forward
      }
      this
    }

    def sonification(implicit tx: S#Tx): Sonification[S] = {
      implicit val itx: InTxn = tx.peer
      if (sonifLoc.isInitialized) sonifLoc.get
      else {
        val sonif = _obj()
        sonifLoc.set(sonif)
        sonif
      }
    }

    def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = {
      logDebugTx("AuralSonification: prepare")
      _procView.prepare(timeRef)
    }

    def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: S#Tx): Unit = {
      logDebugTx("AuralSonification: play")
      _procView.run(timeRef, target)
    }

    def stop()(implicit tx: S#Tx): Unit = {
      logDebugTx("AuralSonification: stop")
      _procView.stop()
    }

    def state(implicit tx: S#Tx): Runner.State = {
      _procView.state
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      procViewL.dispose()
      _procView.dispose()
      // status   .dispose()
    }
  }
}
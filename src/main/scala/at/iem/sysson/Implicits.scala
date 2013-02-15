package at.iem.sysson

import ucar.{nc2, ma2}
import collection.JavaConversions
import de.sciss.synth
import synth.ugen

object Implicits {
  final val all = OpenRange.all
  final val at  = OpenRange.at

  object end    // used in expressions such as 1 to end
  object start {
    def to   (idx: Int):    OpenRange = OpenRange(startOption = None, endOption = Some(idx), isInclusive = true)
    def to   (e: end.type): OpenRange = OpenRange.all
    def until(idx: Int):    OpenRange = OpenRange(startOption = None, endOption = Some(idx), isInclusive = false)
    def until(e: end.type): OpenRange = OpenRange.all
  }

  implicit def sectionAll(v: nc2.Variable): VariableSection = {
    val s = new ma2.Section(v.getShape)
    new VariableSection(v, s)
  }

  implicit class SyRichInt(i: Int) {
    def to   (e: end.type): OpenRange = OpenRange(startOption = Some(i), endOption = None, isInclusive = true)
    def until(e: end.type): OpenRange = OpenRange(startOption = Some(i), endOption = None, isInclusive = false)
  }

  implicit class RichNetcdfFile(peer: nc2.NetcdfFile)
    extends impl.HasDimensions with impl.HasAttributes with impl.HasVariables {

    import JavaConversions._
    def path          = peer.getLocation
    def dimensions    = peer.getDimensions.toIndexedSeq
    def attributes    = peer.getGlobalAttributes.toIndexedSeq
    def rootGroup     = peer.getRootGroup
    def variables     = peer.getVariables.toIndexedSeq
  }

  implicit class RichAttribute(peer: nc2.Attribute) {
    def name          = peer.getName
    def dataType      = peer.getDataType
    def size          = peer.getLength
    def values        = peer.getValues
  }

  implicit class RichGroup(peer: nc2.Group) extends impl.HasDimensions with impl.HasAttributes {
    import JavaConversions._
    def name          = peer.getName
    def attributes    = peer.getAttributes.toIndexedSeq
    def dimensions    = peer.getDimensions.toIndexedSeq
    def variables     = peer.getVariables.toIndexedSeq
    def children      = peer.getGroups.toIndexedSeq
    def parent        = Option(peer.getParentGroup)
  }

  implicit class RichDimension( peer: nc2.Dimension ) {
    def name          = Option(peer.getName)
    def size          = peer.getLength
    def group         = Option(peer.getGroup)
  }

  implicit class RichVariable(peer: nc2.Variable) extends impl.HasDimensions with impl.HasAttributes {
    import JavaConversions._
    def fullName      = peer.getFullName
    def name          = peer.getShortName
    def dataType      = peer.getDataType
    def shape         = peer.getShape.toIndexedSeq
    def size          = peer.getSize
    def rank          = peer.getRank
    def attributes    = peer.getAttributes.toIndexedSeq
    def description   = Option(peer.getDescription)
    def group         = Option(peer.getParentGroup)
    def dimensions    = peer.getDimensions.toIndexedSeq
    def ranges        = peer.getRanges.toIndexedSeq
  }

  implicit class RichArray(peer: ma2.Array) {
    def size          = peer.getSize
    def elementType   = peer.getElementType
    def rank          = peer.getRank
    def shape         = peer.getShape.toIndexedSeq
    def f1d_force: IndexedSeq[Float] = float1d(force = true)
    def f1d: IndexedSeq[Float] = float1d(force = false)

    private def float1d(force: Boolean) = {
      require(peer.getElementType == classOf[Float], "Wrong element type (" + peer.getElementType + "); required: Float")
      if (!force) require(peer.getRank == 1, "Wrong rank (" + peer.getRank + "); required: 1")
      val sz = peer.getSize
      require(sz <= 0x7FFFFFFF, "Array too large (size = " + sz + ")")
      val it = peer.getIndexIterator
      IndexedSeq.fill(sz.toInt)(it.getFloatNext)
    }
  }

  implicit class RichFloatSeq(peer: IndexedSeq[Float]) {
    def replaceNaNs(value: Float): IndexedSeq[Float] = {
      peer.collect {
        case Float.NaN => value
        case x => x
      }
    }

    def dropNaNs: IndexedSeq[Float] = peer.filterNot(java.lang.Float.isNaN)

    def normalize: IndexedSeq[Float] = {
      val sz   = peer.size
      if( sz == 0 ) return peer
      var min  = Float.MaxValue
      var max  = Float.MinValue
      var i = 0; while (i < sz) {
        val f = peer(i)
        if(java.lang.Float.isInfinite(f) || java.lang.Float.isNaN(f)) sys.error("Unbounded value: " + f)
        if (f < min) min = f
        if (f > max) max = f
      i += 1 }
      val div = max - min
      if (div == 0f) return IndexedSeq.fill(sz)(0f)
      assert(div > 0f)
      val mul = 1f / div
      peer.map(f => (f - min) * mul)
    }

    def linlin(srcLo: Double = 0.0, srcHi: Double = 1.0)(tgtLo: Double, tgtHi: Double): IndexedSeq[Float] = {
      require(srcLo != srcHi, "Source range is zero (lo = " + srcLo + ", hi = " + srcHi + ")")
      require(tgtLo != tgtHi, "Target range is zero (lo = " + tgtLo + ", hi = " + tgtHi + ")")
      val add1 = -srcLo
      val mul  = (tgtHi - tgtLo) / (srcHi - srcLo)
      val add2 = tgtLo
      peer.map(f => ((f + add1) * mul + add2).toFloat)
    }

    def asEnv(dur: Double, shape: synth.Env.ConstShape = synth.stepShape): ugen.EnvGen = {
      import synth._
         import ugen._
      val sz = peer.size
      require(sz > 0, "Sequence is empty")
      val segDur  = dur / sz
      val env = Env(peer.head, peer.tail.map(mag => Env.Seg(segDur, mag, shape)) :+ Env.Seg(segDur, peer.last, stepShape))
      EnvGen.ar(env, doneAction = freeSelf)
    }
  }
}
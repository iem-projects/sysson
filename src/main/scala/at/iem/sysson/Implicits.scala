package at.iem.sysson

import ucar.{nc2, ma2}
import collection.JavaConversions
import de.sciss.synth
import synth.ugen
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import java.io.File

object Implicits {
  final val all = OpenRange.all
//  final val at  = OpenRange.at

  object end    // used in expressions such as 1 to end
  object start {
    def to   (idx: Int):    OpenRange = OpenRange(startOption = None, endOption = Some(idx), isInclusive = true)
    def to   (e: end.type): OpenRange = OpenRange.all
    def until(idx: Int):    OpenRange = OpenRange(startOption = None, endOption = Some(idx), isInclusive = false)
    def until(e: end.type): OpenRange = OpenRange.all
  }

//  implicit def sectionAll(v: nc2.Variable): VariableSection = {
//    val s = IIdxSeq.fill(v.rank)(all) // new ma2.Section(v.getShape)
//    new VariableSection(v, s)
//  }

  // important: use name distinct from `RichInt` which would otherwise shadow enrichments from ScalaCollider
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

  implicit class RichVariable(peer: nc2.Variable)
    extends impl.HasDimensions with impl.HasAttributes with impl.VariableLike{

    import JavaConversions._
    def fullName      = peer.getFullName
    def name          = peer.getShortName
    def dataType      = peer.getDataType
    def shape         = peer.getShape.toIndexedSeq
    /** Reports the total number of elements within the variable's matrix */
    def size          = peer.getSize
    def rank          = peer.getRank
    def attributes    = peer.getAttributes.toIndexedSeq
    def description   = Option(peer.getDescription)
    def group         = Option(peer.getParentGroup)
    def dimensions    = peer.getDimensions.toIndexedSeq
    def ranges        = peer.getRanges.toIndexedSeq

    def read()        = peer.read()

    def in(dim: String): VariableSection.In = selectAll.in(dim)

    protected def scale = Scale.Identity

    def selectAll: VariableSection = {
      val s = IIdxSeq.fill(rank)(all)
      new VariableSection(peer, s)
    }

    def applyScale(scale: Scale) = selectAll.applyScale(scale)
  }

  implicit class RichArray(peer: ma2.Array) {
    def size          = peer.getSize
    def elementType   = peer.getElementType
    def rank          = peer.getRank
    def shape         = peer.getShape.toIndexedSeq
//    def f1d_force: IndexedSeq[Float] = float1d(force = true)
//    def f1d: IndexedSeq[Float] = float1d(force = true)

    private def requireFloat() {
      require(peer.getElementType == classOf[Float], s"Wrong element type (${peer.getElementType}); required: Float")
    }

    def scaled1D(scale: Scale): IIdxSeq[Float] = {
      val it = float1DIter
      val sz = peer.getSize
      Vector.fill(sz.toInt)(scale(it.getFloatNext).toFloat)
    }

    def float1D: IIdxSeq[Float] = {
      val it = float1DIter
      val sz = peer.getSize
      Vector.fill(sz.toInt)(it.getFloatNext)
    }

    private def float1DIter: ma2.IndexIterator = {
      requireFloat()
//      if (!force) require(peer.getRank == 1, s"Wrong rank (${peer.getRank}); required: 1")
      val sz = peer.getSize
      require(sz <= 0x7FFFFFFF, s"Array too large (size = $sz)")
      peer.getIndexIterator
    }

    def minmax: (Double, Double) = {
      requireFloat()
      require(peer.getSize > 0, "Array is empty")
      val it  = peer.getIndexIterator
      var min = Float.MaxValue
      var max = Float.MinValue
      while (it.hasNext) {
        val f = it.getFloatNext
        if (f < min) min = f
        if (f > max) max = f
      }
      (min, max)
    }
  }

  implicit class RichFloatSeq(peer: IIdxSeq[Float]) {
    def replaceNaNs(value: Float): IIdxSeq[Float] = {
      peer.collect {
        case Float.NaN => value
        case x => x
      }
    }

    def dropNaNs: IIdxSeq[Float] = peer.filterNot(java.lang.Float.isNaN)

    def normalize: IIdxSeq[Float] = {
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
      if (div == 0f) return Vector.fill(sz)(0f)
      assert(div > 0f)
      val mul = 1f / div
      peer.map(f => (f - min) * mul)
    }

    def linlin(srcLo: Double = 0.0, srcHi: Double = 1.0)(tgtLo: Double, tgtHi: Double): IIdxSeq[Float] = {
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

  implicit class RichFile(f: File) {
    def /(child: String): File = new File(f, child)
    def path: String  = f.getPath
    def name: String  = f.getName
    def parent: File  = f.getParentFile
  }
}
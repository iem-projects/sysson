def calcIndices(off: Int, shape: Vector[Int]): Vector[Int] = {
  val modsDivs = shape zip shape.scanRight(1)(_ * _).tail
  modsDivs.map { case (mod, div) =>
    (off / div) % mod
  }
}

def calcPOI(a: Vector[Int], b: Vector[Int], min: Int): Int = {
  val res = (a.drop(min) zip b.drop(min)).indexWhere { case (ai, bi) => ai != bi }
  if (res < 0) a.size else res + min
}

def zipToRange(a: Vector[Int], b: Vector[Int]): Vector[Range] =
  (a, b).zipped.map { (ai, bi) =>
    require (ai <= bi)
    ai to bi
  }
  
def calcOff(a: Vector[Int], shape: Vector[Int]): Int = {
  val divs = shape.scanRight(1)(_ * _).tail
  (a, divs).zipped.map(_ * _).sum
}

def indexTrunc(a: Vector[Int], poi: Int, inc: Boolean): Vector[Int] =
  a.zipWithIndex.map { case (ai, i) =>
    if      (i < poi) ai
    else if (i > poi) 0
    else if (inc)     ai + 1
    else              ai
  }

var DEBUG = true

def partition(shape: Vector[Int], off: Int, len: Int): List[Vector[Range]] = {
  val rankM   = shape.size - 1
  val shapeSz = shape.product

  def loop(start: Int, stop: Int, poiMin: Int, dir: Boolean, 
           res0: List[Vector[Range]]): List[Vector[Range]] =
    if (start == stop) res0 else {
      assert(start <= stop, "start > stop")
      
      val s0  = calcIndices(start, shape)
      val s1  = calcIndices(stop , shape)
      val poi = calcPOI(s0, s1, poiMin)
      val s1t = if (poi >= rankM) s1 
        else indexTrunc(if (dir) s0 else s1, poi, inc = dir)
      
      val trunc = calcOff(s1t, shape)
      val split = if (dir) trunc != (stop % shapeSz) else trunc != start

      if (DEBUG)
        println(f"[${if (dir) "lo" else "hi"}] start = $start%3d, stop = $stop%3d, s0 = ${indexStr(s0)}; s1 = ${indexStr(s1)} --> poi = $poi, trunc = ${indexStr(s1t)} / $trunc; split $split")
      
      if (split /* s1t != s1 */) { // have to split
        // val trunc = calcOff(s1t, shape)
        if (dir) {
          assert(trunc < stop, "trunc == stop")
          val res1 = loop(start, trunc, poiMin = poi + 1, dir = true , res0 = res0)
          loop           (trunc, stop , poiMin = poi    , dir = false, res0 = res1)
        } else {
          assert(trunc > start, "truncOff == start")
          val s1tm = calcIndices(trunc - 1, shape)
          if (DEBUG) println(s"read from ${indexStr(s0)} to ${indexStr(s1tm)}")
          val res1 = zipToRange(s0, s1tm) :: res0
          loop           (trunc, stop , poiMin = poi + 1, dir = false, res0 = res1)
        }
      } else {
        val s1m = calcIndices(stop - 1, shape)
        if (DEBUG) println(s"read from ${indexStr(s0)} to ${indexStr(s1m)}")
        zipToRange(s0, s1m) :: res0
      }
    }
  
  loop(off, off + len, poiMin = 0, dir = true, res0 = Nil).reverse
}

def partSize(in: List[Vector[Range]]): Int = in.map(_.map(_.size).product).sum

val sz = Vector(2, 3, 4, 5)
val x0 = partition(shape = sz, off = 0, len = 120)
assert(partSize(x0) == 120)

val x1 = partition(shape = sz, off = 0, len = 6)
assert(partSize(x1) == 6)

val x2 = partition(shape = sz, off = 6, len = 16)
assert(partSize(x2) == 16)

val x3 = partition(shape = sz, off = 6, len = 21)
assert(partSize(x3) == 21)

def printResult(res: List[Vector[Range]]): Unit = {
  val s = res.map(v => v.map(r => Vector(r.head, r.last)).transpose
    .map(_.mkString("[", ", ", "]")).mkString("\n"))
    .mkString("// read\n", "\n// read\n", "")
  println(s)
}

printResult(x1)
printResult(x2)
printResult(x3)

///////////////////////////


class Variable[A](val data: Vector[A], val shape: Vector[Int]) {
  require(data.size == shape.product)
  
  def read(sections: Vector[Range]): Vector[A] = {
    require(sections.size == shape.size)
    require(sections.zipWithIndex.forall { case (r, ri) => r.forall(i => i >= 0 && i < shape(ri)) })
    
    val sz  = if (sections.isEmpty) 0 else (1 /: sections)(_ * _.size)
    val zip = (shape zip sections).reverse
    Vector.tabulate(sz) { i =>
      val (j, _, _) = ((0, 1, 1) /: zip) { case ((res, m, n), (dim, r)) =>
        val add = r((i / n) % r.size) * m
        (res + add, m * dim, n * r.size)
      }
      data(j)
    }
  }
}

val v = new Variable[Int](Vector(1 to 4096: _*), Vector(8, 8, 8, 8))

trait ChunkReader[A] {
  def read(chunkSize: Int): Vector[A]
}

def sampleRange(in: Range, by: Range): Range = {
  val drop  = by.start
  val stepM = by.step
  require(drop >= 0 && stepM > 0)
  val in1 = in.drop(drop)
  val in2 = if (stepM == 1)
    in1
  else if (in1.isInclusive)  // copy-method is protected
    new Range.Inclusive(start = in1.start, end = in1.end, step = in1.step * stepM)
  else
    new Range(start = in1.start, end = in1.end, step = in1.step * stepM)
  in2.take(by.size)
}

class Impl[A](val v: Variable[A], val in: Vector[Range]) 
  extends ChunkReader[A] {
    
  require(in.size == v.shape.size)
 
  private[this] var _pos    = 0
  private[this] val inShape = in.map(_.size)
  
  def pos : Int = _pos
  val size: Int = inShape.product
  
  def read(len: Int): Vector[A] = {
    require (len >= 0 && _pos + len <= size)
    
    val sub = partition(shape = inShape, off = _pos, len = len)
    val res: Vector[A] = sub.flatMap { ranges =>
      val sec = (in, ranges).zipped.map(sampleRange)
      v.read(sec)
    } (collection.breakOut)
    _pos += len
    res
  }
}

def test(sec: Vector[Range], chunk: Int): Vector[Int] = {
  val impl = new Impl(v, sec)
  val b = Vector.newBuilder[Int]
  var start = 0
  val stop = sec.map(_.size).product
  while (start < stop) {
    val len = math.min(stop - start, chunk)
    if (DEBUG) println(s"read(pos = ${impl.pos}, len = $len")
    b ++= impl.read(len)
    start += len
  }
  b.result()
}

DEBUG = false
test(Vector(0 to 0, 0 to 0, 0 to 0, 0 to 0), 10)
test(Vector(0 to 0, 0 to 0, 0 to 0, 1 to 1), 10)
test(Vector(0 to 0, 0 to 0, 1 to 1, 0 to 0), 10)
test(Vector(0 to 0, 1 to 1, 0 to 0, 0 to 0), 10)
test(Vector(1 to 1, 0 to 0, 0 to 0, 0 to 0), 10)
test(Vector(0 to 0, 0 to 0, 0 to 0, 0 to 7), 10)

test(Vector(0 to 0, 0 to 0, 0 to 1, 0 to 5), 10)

val sh1 = Vector(1, 1, 2, 6)
val x4 = partition(shape = sh1, off = 10, len = 2)
assert(partSize(x3) == 21)

def printIndices(off: Int, len: Int, shape: Vector[Int]): Unit =
  (off until (off + len)).map(calcIndices(_, shape))
    .map(_.mkString("[", ", ", "]")).foreach(println)

printIndices(off = 10, len = 2, shape = sh1)

test(Vector(0 to 0, 0 to 0, 0 to 1, 0 to 6), 14)

////////////////

val sh2 = Vector(1, 1, 1, 2)
DEBUG = true
val x5 = partition(shape = sh2, off = 1, len = 1)
assert(partSize(x3) == 21)

/////////////////

val sh3 = Vector(1, 2, 2, 2)
printIndices(off = 5, len = 3, shape = sh3)
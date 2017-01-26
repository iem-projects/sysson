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

def partition(shape: Vector[Int], off: Int, len: Int): List[Vector[Range]] = {
  val rankM = shape.size - 1

  def loop(start: Int, stop: Int, poiMin: Int, dir: Boolean, 
           res0: List[Vector[Range]]): List[Vector[Range]] =
    if (start == stop) res0 else {
      assert(start <= stop, "start > stop")
      
      val s0  = calcIndices(start, shape)
      val s1  = calcIndices(stop , shape)
      val poi = calcPOI(s0, s1, poiMin)
      val s1t = if (poi >= rankM) s1 
        else indexTrunc(if (dir) s0 else s1, poi, inc = dir)
      
//       println(f"[${if (dir) "lo" else "hi"}] start = $start%3d, stop = $stop%3d, s0 = ${indexStr(s0)}; s1 = ${indexStr(s1)} --> poi = $poi, floor = ${indexStr(s1t)}")
      
      if (s1t != s1) { // have to split
        val trunc = calcOff(s1t, shape)
        if (dir) {
          assert(trunc < stop, "trunc == stop")
          val res1 = loop(start, trunc, poiMin = poi + 1, dir = true , res0 = res0)
          loop           (trunc, stop , poiMin = poi    , dir = false, res0 = res1)
        } else {
          assert(trunc > start, "truncOff == start")
          val s1tm = calcIndices(trunc - 1, shape)
          val res1 = zipToRange(s0, s1tm) :: res0
          loop           (trunc, stop , poiMin = poi + 1, dir = false, res0 = res1)
        }
      } else {
        val s1m = calcIndices(stop - 1, shape)
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

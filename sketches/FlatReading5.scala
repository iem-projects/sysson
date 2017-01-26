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
      val s0  = calcIndices(start, shape)
      val s1  = calcIndices(stop , shape)
      val poi = calcPOI(s0, s1, poiMin)
      val ti  = if (dir) s0 else s1
      val to  = if (dir) s1 else s0
      val st  = if (poi >= rankM) to else indexTrunc(ti, poi, inc = dir)
            
      val trunc = calcOff(st, shape)
      val split = trunc != (if (dir) stop else start)
      if (split) {
        if (dir) {
          val res1 = loop(start, trunc, poiMin = poi + 1, dir = true , res0 = res0)
          loop           (trunc, stop , poiMin = poi    , dir = false, res0 = res1)
        } else {
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

partition(sz, 0, 6)
partition(sz, 6, 21)

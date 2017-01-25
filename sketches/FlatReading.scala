type Vec[+A] = collection.immutable.Vector[A]
val  Vec     = collection.immutable.Vector

class Variable[A](data: Vec[A], val shape: Vec[Int]) {
  require(data.size == shape.product)
  
  def read(sections: Vec[Range]): Vec[A] = {
    require(sections.size == shape.size)
    require(sections.zipWithIndex.forall { case (r, ri) => r.forall(i => i >= 0 && i < shape(ri)) })
    
    val sz  = if (sections.isEmpty) 0 else (1 /: sections)(_ * _.size)
    val zip = (shape zip sections).reverse
    Vec.tabulate(sz) { i =>
      val (j, _, _) = ((0, 1, 1) /: zip) { case ((res, m, n), (dim, r)) =>
        val add = r((i / n) % r.size) * m
        (res + add, m * dim, n * r.size)
      }
      data(j)
    }
  }
}

val v = new Variable[Int](Vec(1 to 512: _*), Vec(8, 8, 8))
    
assert(v.read(Vec(0 until 0, 0 until 0, 0 until 0)) == Vec())
assert(v.read(Vec(0 to 0, 0 to 0, 0 to 2)) == Vec(1, 2, 3))
assert(v.read(Vec(1 to 1, 1 to 1, 0 to 2)) == Vec(73, 74, 75))

val selSome = Vec(
  2 until 4,  // size 2
  3 until 6,  // size 3
  4 until 8   // size 4
)

assert(v.read(selSome) == Vec(
  157, 158, 159, 160, 
  165, 166, 167, 168, 
  173, 174, 175, 176, 
  
  221, 222, 223, 224, 
  229, 230, 231, 232, 
  237, 238, 239, 240
))

trait ChunkReader[A] {
  def read(chunkSize: Int): Vec[A]
}

def test(c: ChunkReader[Int]): Unit =
  assert((1 to 4).map(c.read(5)) ++ c.read(4) == Vec(1 to 24: _*))

// test(???)

def calcInSection(pos: Int, sections: Vec[Range]): Vec[Int] = {
  val sizes    = sections.map(_.size)
  val modsDivs = sizes zip sizes.scanRight(1)(_ * _).tail
  modsDivs.map { case (mod, div) =>
    (pos / div) % mod
  }
}

val selAll = v.shape.map(0 until _)
calcInSection(1, selAll)

(0 until 24).map(i => calcInSection(i, selAll)).foreach(println)

//////////////////////////////////////////////

val selSome = Vec(
  2 until 4,  // size 2
  3 until 6,  // size 3
  4 until 8   // size 4
)

calcInSection( 0, selSome)  // [0 0 0]
calcInSection( 4, selSome)  // [0 1 0]

calcInSection( 5, selSome)  // [0 1 1]
calcInSection( 9, selSome)  // [0 2 1]

// this is the most interesting case as the highest dimension index changes
// ; can we do this with two reads?
calcInSection(10, selSome)  // [0 2 2]
calcInSection(14, selSome)  // [1 0 2]

calcInSection(15, selSome)  // [1 0 3]
calcInSection(19, selSome)  // [1 1 3]

// this is an interesting case because only the lowest dim index changes
// ; this should be solved with a single read
calcInSection(20, selSome)  // [1 2 0]
calcInSection(23, selSome)  // [1 2 3]

/*---

Algorithm:

- s0 := calculate the start indices for current `pos`, as done above through `calcInSection`
- i0 := in s0, find the right-most dim index where the index (elem in s0) is > 0; if there is none, i0 := 0
- numProc := min((sizes(i0) - s0(i0)) * mods(i0), chunkRemain)
- ...

let's see this attempt in the above example (reading 4 times 5 elements, then 4):

*/

(pos = 0, chunk = 5) --> s0 := [0 0 0]; i0 := 0; numProc = min(2 - 0) * 12 = 24, 5) = 5

// that doesn't help; we should also look at s1 (stop indices)

val expected = v.read(selSome)
assert(expected.size == expected.distinct.size)

// e.g. (2, 3, 4) --> 4 + 3 * 8 + 2 * 64   + 1 = 157 OK
// e.g. (3, 5, 7) --> 7 + 5 * 8 + 3 * 64   + 1 = 240 OK

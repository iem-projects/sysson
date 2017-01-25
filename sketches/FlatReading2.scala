def calcIndices(pos: Int, sizes: Vector[Int]): Vector[Int] = {
  val modsDivs = sizes zip sizes.scanRight(1)(_ * _).tail
  modsDivs.map { case (mod, div) =>
    (pos / div) % mod
  }
}

val sz  = Vector(2, 3, 4, 5)
val num = sz.product  // 120

def printIndices(range: Range): Unit =
  range.map(calcIndices(_, sz)).map(_.mkString("[", ", ", "]")).foreach(println)

printIndices(0 until num)

[0, 0, 0, 0]
[0, 0, 0, 1]
[0, 0, 0, 2]
[0, 0, 0, 3]
[0, 0, 0, 4]
[0, 0, 1, 0]
[0, 0, 1, 1]
[0, 0, 1, 2]
[0, 0, 1, 3]
[0, 0, 1, 4]
[0, 0, 2, 0]
[0, 0, 2, 1]
[0, 0, 2, 2]
[0, 0, 2, 3]
[0, 0, 2, 4]
[0, 0, 3, 0]
[0, 0, 3, 1]
[0, 0, 3, 2]
[0, 0, 3, 3]
[0, 0, 3, 4]
[0, 1, 0, 0]
...
[1, 1, 3, 4]
[1, 2, 0, 0]
[1, 2, 0, 1]
[1, 2, 0, 2]
[1, 2, 0, 3]
[1, 2, 0, 4]
[1, 2, 1, 0]
[1, 2, 1, 1]
[1, 2, 1, 2]
[1, 2, 1, 3]
[1, 2, 1, 4]
[1, 2, 2, 0]
[1, 2, 2, 1]
[1, 2, 2, 2]
[1, 2, 2, 3]
[1, 2, 2, 4]
[1, 2, 3, 0]
[1, 2, 3, 1]
[1, 2, 3, 2]
[1, 2, 3, 3]
[1, 2, 3, 4]

val r1 = 0 until 6
printIndices(r1)

// first read
[0, 0, 0, 0]
[0, 0, 0, 1]
[0, 0, 0, 2]
[0, 0, 0, 3]
[0, 0, 0, 4]

// second read
[0, 0, 1, 0]

expected result:

val res1 = List(
  Vector(0 to 0, 0 to 0, 0 to 0, 0 to 4),
  Vector(0 to 0, 0 to 0, 1 to 1, 0 to 0)
)

assert(res1.map(_.map(_.size).product).sum == r1.size)

val r2 = 6 until 22
printIndices(r2)

// first read
[0, 0, 1, 1]
[0, 0, 1, 2]
[0, 0, 1, 3]
[0, 0, 1, 4]

// second read
[0, 0, 2, 0]
[0, 0, 2, 1]
[0, 0, 2, 2]
[0, 0, 2, 3]
[0, 0, 2, 4]
[0, 0, 3, 0]
[0, 0, 3, 1]
[0, 0, 3, 2]
[0, 0, 3, 3]
[0, 0, 3, 4]

// third read
[0, 1, 0, 0]
[0, 1, 0, 1]

expected result:

val res2 = List(
  Vector(0 to 0, 0 to 0, 1 to 1, 1 to 4),
  Vector(0 to 0, 0 to 0, 2 to 3, 0 to 4),
  Vector(0 to 0, 1 to 1, 0 to 0, 0 to 1)
)

assert(res2.map(_.map(_.size).product).sum == r2.size)

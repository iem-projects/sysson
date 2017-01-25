refs:

- https://gist.github.com/Sciss/743d69e26710cda3587e9ccaedf064f2
- https://gist.github.com/Sciss/cb44d425dfce564c32ac7017d115bef7


- "drop" dimensions from left which
  remain constant from s0 to s1
- e.g. s0 = [0, 0, 1, 1] and s1 = [0, 1, 0, 1]
  then we have a constant prefix
  [0] and truncated suffix indices
  s0t = [0, 1, 1] and s1t = [1, 0, 1]


// first hypercube or read
[0, 1, 1] := s0t
[0, 1, 2]
[0, 1, 3]
[0, 1, 4]
(stop index = 2 * 5; len = 10 - 6 = 4)

// second hypercube or read
[0, 2, 0]
[0, 2, 1]
[0, 2, 2]
[0, 2, 3]
[0, 2, 4]
[0, 3, 0]
[0, 3, 1]
[0, 3, 2]
[0, 3, 3]
[0, 3, 4]
(stop index = 4 * 5; len = 20 - 10 = 10)

// third hypercube or read
[1, 0, 0]
[1, 0, 1] := s1t

// ^^^ hier ist das das ende,
// aber generell muss hier ein ruecklaeufiger
// prozess folgen

/////////////// beispiel, das ruecklaeufigen
// prozess benoetigt: s1t := [1, 1, 1] d.h.
// fuef elemente mehr:

val off3 = 6
val len3 = 16 + 5

def printIndices(off: Int, len: Int): Unit =
  (off until (off + len)).map(calcIndices(_, sz))
    .map(_.mkString("[", ", ", "]")).foreach(println)
    
printIndices(off3, len3)

// read 1
[0, 0, 1, 1]
[0, 0, 1, 2]
[0, 0, 1, 3]
[0, 0, 1, 4]

// read 2
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

// "mittelachse"

// read 3
[0, 1, 0, 0]
[0, 1, 0, 1]
[0, 1, 0, 2]
[0, 1, 0, 3]
[0, 1, 0, 4]

// read 4
[0, 1, 1, 0]
[0, 1, 1, 1]

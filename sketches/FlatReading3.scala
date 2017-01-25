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

// shape: [2, 3, 4, 5]

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

///////////////////////
///////////////////////

- mittel-achsen wert ist
  "ceil" fuer left-most
  dimension die sich veraendert
  
  d.h. fuer off = 6, len = 21,
  mit s0 = [0, 0, 1, 1] und
  s1 = [0, 1, 1, 1] dieser ceil
  wert ist [0, *, _, _] = [0, 1, 0, 0]
  
wir haben eine sub region mit start
 [0, 0, 1, 1]
und stop
 [0, 1, 0, 0]
bzw. last
 [0, 0, 3, 4]
 
jetzt wird der prozess rekursiv wiederholt. neues ceil ist
 [*, _] = [2, 0]
 
sub region mit start
 [0, 0, 1, 1]
und stop
 [0, 0, 2, 0]
bzw. last
 [0, 0, 1, 4]

ende --> first read [0, 0, 1, 1] to [0, 0, 1, 4]

(ohne weitere pruefung)

ende --> second read [0, 0, 2, 0] to [0, 0, 3, 4]

// zweiter teil; symmetrisch: "floor" berechnung

ausgangspunkt ist die spanne: 

[0, 1, 0, 0]
[0, 1, 1, 1]

versuch: left-most dimension die sich veraendert
ist der punkt [0, 1, *, _]

was waere beim vorherigen fall mit len = 16?

die spanne waere
[0, 1, 0, 0]
[0, 1, 0, 1]

und derleft-most dimension punkt waere
[0, 1, 0, *] finito

makes sense...

ok. also: len = 21, punkt ist  [0, 1, *, _]
dann waere floor: [0, 1, 1, 0]

wir haben wieder "oberhalb" damit die sub region mit start
 [0, 1, 0, 0]
und stop
 [0, 1, 1, 0]
bzw. last
 [0, 1, 0, 4]

der point-of-interest liegt ganz am ende, also sind wir hier fertig

ende --> third read [0, 1, 0, 0] to [0, 1, 0, 4]

// wieder zurueck zum "unteren" teil

ausgangspunkt ist die spanne: 

[0, 1, 1, 0]
[0, 1, 1, 1]

point of interest ist letztes element, also sind wir fertig

ende --> fourth read [0, 1, 1, 0] to [0, 1, 1, 1]

// damit ist worst case O(2 * rank) ?

///////////////////////////////////////
///////////////////////////////////////

- gegebene index "spanne" bzw. tuple (s1, s2)

was waere mit

[0, 0, 0, 0]
[1, 1, 1, 1]

dies sollte ja in einem read moeglich sein?

printIndices(0, 87)

hah! denkfehler. ist _nicht in einem read moeglich. nur das gesamte array waere in einem moeglich:

[0, 0, 0, 0]
[1, 2, 3, 4]

bzw. mit stop (exclusive) codierung:

[0, 0, 0, 0]
[2, 0, 0, 0]

wenn wir von 'stop' ausgehen, ist der point-of-interest

[*, _, _, _]

damit bleibt die floor region identisch

(oder wir 'wrappen' den wert um so dass beide [0, 0, 0, 0] sind und damit kein POI;
 das wuerde ja eh mit calcIndices schon passieren: calcIndices(120, sz) == Vector(0, 0, 0, 0) !)


//////////////////////////////////////////////


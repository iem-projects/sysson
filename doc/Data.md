# Data Management

The following examples assume that SysSon is launched in console mode: `sbt console`.

## NetCDF Files

```scala

    val f = open("<path-to-file>")  // opens a NetCDF file for the given path
    f.path              // the file path
    f.dimensions        // a vector of the dimensions in the file
    f.attributes        // a vector of the attributes in the file
    f.rootGroup         // the top level group in the file
    f.variables         // a vector of the variables in the file

    f.dimensions(0)     // the first dimension in the file
    f.variables(0)      // the first variable in the file

    f.dimensionMap      // a dictionary from names to dimensions
    f.attributeMap      // a dictionary from names to attributes
    f.variableMap.keys  // a set of variable names
    f.variableMap       // a dictionary from names to variables

    f.dimensionMap("lat")   // gets the dimension named "lat" (latitude)
    f.attributeMap("Title") // gets the attribute named "Title"
    f.variableMap("temp")   // gets the variable named "temp"
```

## Dimensions

```scala

    val d1 = f.dimensionMap("lat")
    d1.name  // the dimension's name (optional)
    d1.size  // the dimension's size (number of samples in that dimension)
```

## Attributes

```scala

    val a1 = f.attributeMap("Title")
    a1.name         // attribute name (key)
    a1.dataType     // type of data stored under the attribute key, e.g. `String`
    a1.size         // number of elements stored
    a1.data         // array of the attribute elements
```

## Groups

```scala

    val g1 = f.rootGroup
    g1.name         // group's name
    g1.children     // vector of nested child groups within this group
    g1.parent       // parent group of which this group is a child (optional)
```

The group furthermore responds to the same method as the file, such as getting attributes (`attributes`, `attributeMap`), variables (`variables`, `variableMap`) and dimensions (`dimensions`, `dimensionMap`).

## Variables

Variables describe an n-dimensional matrix of sampled data along with a name and dimensions associated with the matrix.

```scala

    val v1 = f.variableMap("refr")
    v1.dataType      // type of the matrix elements, e.g. `float`
    v1.rank          // matrix rank, e.g. `3`
    v1.shape         // matrix shape which is a vector of the sizes of each dimension, e.g. `Vector(108, 10, 18)`
    v1.size          // total number of elements in the matrix, i.e. the product of the shape; e.g. `19440`
    v1.group         // group in which the variable resides (optional)
    v1.description   // textual description of the variable (optional)
```

The variable furthermore responds to the common methods `attributes`, `attributeMap`, `dimensions`, `dimensionMap`, e.g. `v1.attributeMap("missing_value")`.

## Variable Sections

Often you want to select sub-sections of the matrix, for example to reduce the dimensionality. To make a section, the dimension must first be given by its name, followed by a range selection statement. Examples:

```scala

    val sel1 = v1 in "time" select 0  // selects the time slice at time index 0
    sel1.rank                // e.g. `3` (same as original variable)
    sel1.size                // total elements in the sub matrix, e.g. `180`
    sel1.reducedRank         // after eliminating dimensions of size 1, e.g. `2`
    sel1.reducedShape        // after eliminating dimensions of size 1
    sel1.reducedDimensions   // after eliminating dimensions of size 1

    val sel2 = sel1 in "lat" select (0 until 10)         // make another selection
    val sel3 = sel2 in "lat" select (10 to end)          // replaces previous selection
    val sel4 = sel3 in "lat" select (start to 4)         // replaces previous selection
```

Indices always start at zero. Ranges in the form of `(<start> to <end>)` are inclusive (end index is included), whereas ranges in the form of `(<start> until <end>)` are exclusive, as is standard in Scala's regular ranges.

There are also strides, in the form of `(<start> to <end> by <step>)`. However, there seems to be currently a problem with the underlying `Section` type not supporting them properly.

## Reading Data

The whole matrix of a variable may be read through `v1.read`. To read sub sections, make a selection first as described above. For example:

```scala

    val sel = v1 in "time" select 10
    val arr1 = sel.read()
    arr1.shape                  // the shape of the read matrix, e.g. `1 x 10 x 18`
    val arr2 = arr1.reduce      // eliminate dimensions of size 1
    arr2.shape                  // e.g. is now `10 x 18`
    val data = arr2.f1d         // force matrix into a 1-dimensional float vector
```

When using `f1d`, a multi-dimensional array is scanned along the order of its dimensions "breadth-first". For example, if the shape is `2 x 3 x 4`, the first two elements scan the first dimension where dimensions 2 and 3 are fixed at index 0, followed by the two elements in the first dimension where the second dimension is fixed at index 1 and the third dimension is fixed at index 0, etc.

## Sending Data via OSC

To quickly hook up SuperCollider, PD, or ChucK, bulk data can be sent via OSC. The following illustrates this by sending data to the SuperCollider language.

```scala

    import OSCSupport._
    target = localhost -> 57120  // define OSC target (UDP) -- `(localhost, 57120)` is the default

    val sel = f.variableMap("refr") in "time" select 0
    sel.send()
```

The format of the message sent:

    "/sysson_matrix"
    (String) variable-name
    (Int) rank
    [ Dimension-Info ] * rank
    (Int) size
    [ (Float) matrix-element ] * size

Where `Dimension-Info` is defined as follows:

    (String) dimension-name
    (Int) dimension-size (total length, not selection)
    (Int) range start-index in dimension
    (Int) range end-index in dimension (inclusive)
    (Int) range stride (or `1` if no stride is used)

On the SuperCollider language side, you could await such a message as follows:

    ~osc = OSCFunc({ arg msg, time, addr; msg.postln }, "/sysson_matrix");
    ~osc.free;  // to stop listening

To send arbitrary OSC messages:

    send("/test", "one", 2, 3.4)

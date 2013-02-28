# Sonification

There is currently a very simple interface for the data sonification. It is based on the idea that any multi dimensional variable can be broken down (through slicing) into either one dimensional vectors or two dimensional matrices. Of these, each dimension may be mapped to a spatial or temporal parameter in the sonification.

A temporal parameter is one, where the vector or matrix dimension is unrolled in time at a given rate or over a given total duration.

A spatial parameter is one which is used in SuperCollider style multi-channel expansion. That is to say, a vector is used as a multi element UGen input which expands that particular UGen, such as generating a series of oscillators tuned to different frequencies or amplitudes, etc.

## Programming interface

The black box object for sonification is type `Sonification`. It stores a list of parameters _defined by the sonification model itself_, along with constraints for these parameters. A second list then maps between variable sections and these parameters. The sectioning of variables has been already covered in the document [Data.md](Data.md). Given such a `VariableSection`, it must now be specially selected as a sonification source. A thorough example is found in the test-source `SonifTest`. Here is a brief introduction:

```scala

    AudioSystem.start()                     // start the SuperCollider server

    val f = open(<someNetCDFFile>)
    val v = f.variableMap(<someVariable>) in <someDimension> select <someRange>

    val son = Sonification("Test-Son")      // create a sonification handler
    son.graph = {                           // set up a UGen graph
      val data  = MatrixIn.ar("vec")        // MatrixIn connects to a named parameter
      val clip  = data.max(0).min(1)
      val scale = clip.linexp(0, 1, 100, 10000)
      val nr    = data.numRows              // the MatrixIn object responds to numRows and numColumns
      val sin   = SinOsc.ar(scale) * nr.reciprocal.sqrt
      Mix(sin)
    }

    son.matrices += "vec" -> MatrixSpec()   // define a parameter named "vec" -- MatrixSpec is not yet developed!
    val source = v.asMatrix(row = "time", column = "lat")   // define how each matrix dimension is interpreted
    son.mapping += "vec" -> source          // connect the source to a given parameter
    son playOver 10.seconds                 // play the sound for a given duration

```

Note that the basic DSL is provided by importing the `sysson` package and `Implicits`, the UGen generation is made possible through import of `synth._` and `ugen._`, and the notation for durations (`10.seconds`) is made possible by importing `concurrent.duration._` (see `SonifTest.scala`).

The mapping from variable section to sonification is done through one of the three methods `asColumn`, `asRow`, and `asMatrix`. In the former two cases, the section must already reduce to one dimension (perhaps having successively applied `in <dim> select <range>` selections), in the last case it must have been reduced to two dimensions and the names of the row and column dimensions have to be passed into `asMatrix`.

## TODO

 - allow easy buffer management (e.g. using sound files as sources for granulation)
 - add MVC to `Sonification`, so the instances can be managed (and disposed of) in the GUI
 - design the `MatrixSpec` type
 - add global control room management (volume control, panoramic positioning, limiter, meters, headphones bus ...)

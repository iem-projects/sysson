# Sonification (2)

The second iteration of the design has introduced a couple of changes. The `Sonification` type is now defined by a `Patch`, a `variableMap`, and a `userValueMap`.

A `Patch` is simply an association of a synth graph with a name. Multiple new graph elements have been introduced and are discussed below.

The `variableMap` contains mappings between logical names and variable sections used as data source. The GUI currently assigns the plotted variable as `DefaultVariable` (`"<default>"`). Inside the graph function, variables are accessed via `Var()`.

The `userValueMap` contains arbitrary scalar parameters. These would probably presented in the GUI in the form of widgets such as sliders, dials etc. Their function is to customise the sonification independently of data sources chosen. For example, playback speed could be specified this way.

## Library

There is a preliminary idea to manage the sonification patches in the form of a `Library`. Currently, a `TestLibrary` is directly included in the source code and brought up in the GUI via `File -> New -> Library` (Cmd-L). A library is a tree of patches. A next version could include an online editor for the patches.

## Graph Elements

__Note__: This is partially obsolete.

The initial hook to get hold of a variable is the `Var` element. Currently, there is only a no-arg constructor which uses the logical name `Sonification.DefaultVariable`:

    val v = Var()  // get a handle on the default sonification variable

A variable handle is a "graph element", but not yet a `GE`, i.e. it doesn't represent a digital signal yet and cannot be used as input to UGens.

Like the client-side `VariableSection`, the variable handle can be constrained to particular sub matrices.

    val r1  = SelectedRange(Latitude)
    val r2  = SelectedRange(Pressure)
    val sel = v.select(r1, r2)

Ranges can currently not be directly specified, but the `SelectedRange` element provides a handle to user-defined ranges. Here, `Latitude` and `Pressure` are so-called variable _references_. They are logical representations of variables or dimensions. (_Note_: Perhaps he concepts of `Var` and `VarRef` will be unified)

Other pre-defined variable references are `Longitude`, `Time`, `Altitude`.

A signal can be either constant or dynamic. A constant signal is `v.values`. No matter what the shape of the matrix is, you will get all values at once. The `.axis` method lets you view particular slices of the matrix on the server side. A dynamic signal is `v.play(pr)` where `pr` is a "playing-range". A playing range is a range unrolled in time at a given frequency, such as

    val r3   = SelectedRange(Time)
    val pr   = r3.play(10)   // ten data-time steps per second
    val data = sel.play(pr)  // associate the playing time axis as the temporal pointer for the matrix itself

Currently, sample-and-hold is performed, but it will be easy to integrate linear or cubic interpolation.

Next you will most likely want to associate the axes coordinates with the data. While `r1.values` _does_ give the latitudes to you, these values are in no sense _aligned_ with the data section. In order to get a `GE` which plays nicely with the data `GE` in terms of multichannel wrapping, you get different handle on the axis via the variable section:

    val lats  = data.axis(Latitude).values  // latitude values formatted to go with `data`
    val dust  = Dust.ar(data)
    val flt   = Resonz.ar(dust, lats.linexp(-90, 90, 200, 10000))

That is, here the data points, which drive `Dust` generators, are correctly wired to resonant bandpass filters based on the latitudes of these data points.

The only exception is the axis which is played in time, as obviously here no multichannel alignment is required. Thus `pr` can be used as a dynamic signal directly.

### To-Do

The API is already larger than shown above, with methods for getting minimum and maximum values, indices etc. Currently, ranges must be calculated on the server side, for example:

    val latAxis   = r1.values
    val latMin    = Reduce.min(latAxis)
    val latMax    = Reduce.max(latAxis)

This is more costly than calculating them in advance, especially since with large matrices a lot of UGens are required to reduce the values.

## User Interface

The GUI for variable plots now has a drop location in the bottom-left (indicated by a "target" icon), onto which a patch from the library can be dragged. When the drag-and-drop is complete, for each `SelectedRange` element in the patch, the corresponding variable/dimension in the plot receives additional range slider handles with which to adjust the range. The patch name is shown along with transport buttons and user value elements (if any defined).

For example, to define a user value named "speed":

    val speed = UserValue(key = "speed", default = 1.0)

To use this value in the synth graph, `speed.value` is called.

## To-Do

- Apart from completing the API implementation, the overall API should be streamlined and simplified. There should be public types corresponding to each stage of manipulation (e.g. `Var().select(...) <-> Var.Reduction(Var(), ...)`). This will be mandatory when serialising patches.
- One should be able to define and edit patches in the application instead of having to go back to the IDE and re-compile the project.
- Therefore, there will be patch serialisation, perhaps storing the source code as well, and integrating with the REPL.
- Audio file data caching is probably useful, although I have the perception that NetCDF already does the main part of this by itself.
- The audio output should be recordable. To allow this offline, we should integrate Lucre-Synth (which is the lowest layer of SoundProcesses and has now been factored out).
- We need decimation operators, such as averaging, median, standard deviation etc., but also the scanning operators which reduce dimensionality. It might be good to be able to unroll multiple slices in time.
- Visual feedback of where the playback is, as well as pausing and cueing without expensive re-calculation of the data.
- Synchronising multiple plots with different selections. The online sonifications should appear as proxies in the GUI so that we can "arrange" them, e.g. pan two of them to opposite sides etc.
- Axis, guide lines and threshold trigger elements. A simplification could be to put them all into the same Synth or a group of paused Synths, and just trigger these online Synths (i.e., no dynamic scheduling of new synths).
- GUI: should think about some form of docking framework
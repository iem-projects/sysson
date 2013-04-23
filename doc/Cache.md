# Cache

There is a statistics cache on a per file basis. This contains minimum, maximum, mean and standard deviation values across each variable in the file.

An entry is obtained via `Stats.get(document)`. The return type of this function is `Future[Stats]` where `Stats` is a dictionary from variable names to type `Stats.Variable`. This second type contains a field `total` with the overall statistics of type `Count`, as well as a field `slices` which maps from sectioning variable name to a vector of `Count`.

A `Future` has method `.value` which returns an `Option[Try[Stats]]`. If the cache is not yet calculated, this value will be `None`, otherwise `Some`. The contents of `Some` is either a `Success[Stats]` or a `Failure` indicating that something went wrong (e.g. I/O error). The cumbersome way to unwind a stats entry is

```scala

    Stats.get(document).value.get.get
```

This will fail if the cache is not complete upon invoking this call. As a shorthand, a double-bang `!!` method is provided:

```scala

    val stats = Stats.get(document).!!
    val min   = stats.total.min
    val max   = stats.total.max
    val mean  = stats.total.mean
```

If the variable for example contains a dimension `plev` for different pressure levels, the minimum and maximum in the first pressure level would be obtained via

```scala

    val inPlev0 = stats.slices("plev")(0)
    (inPlev0.min, inPlev0.max)
```

The mean magnitude in the second pressure level:

```scala

    stats.slices("plev")(1).mean
```

## Variable Sections

Stats can be directly retrieved for a given variable section:

```scala

    val sec      = myVariable in "lat" select (0 to 10)
    val secStats = sec.stats.!!
```

## Disk location

The caches are located in `$SYSSON_HOME/data/cache`. The cache is truncated to 1 MB at the moment.

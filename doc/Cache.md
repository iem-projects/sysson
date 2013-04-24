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

__Note__: If there are sections across multiple dimensions, the stats cover a _larger_ area than `sec` itself: They reflect the union set of all orthogonal per-dimension sections. For example, if there are 17 pressure levels and 72 latitudes, getting the stats for a section which selects `plev` from 4 to 6 and `lat` from 10 to 19 yields an object which is the union of the slice of `plev` from 4 to 6 across all latitudes with the slice of `lat` from 10 to 19 across all pressure levels. So the `mean`, `min`, `max` etc. values specifically are not the mean, min and max of the sectioned `3 * 10 = 30` values, but actually of the strips whose cross section represents those 30 values. The strips in this example have a combined size of `3 * 72 + 10 * 17 - 3 * 10 = 356`.

## Disk location

The caches are located in `$SYSSON_HOME/data/cache`. The cache is truncated to 1 MB at the moment.

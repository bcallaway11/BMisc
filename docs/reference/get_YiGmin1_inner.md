# get_YiGmin1_inner

Calculates a units outcome (or also can be used for a covariate) in the
period right before it becomes treated. The unit's group must be
specified at this point. This function operates on a data.frame that is
already local to a particular unit.

## Usage

``` r
get_YiGmin1_inner(this_df, yname, tname, gname)
```

## Arguments

- this_df:

  a data.frame, for this function it should be specific to a particular
  unit

- yname:

  name of column containing the outcome (or other variable) for which to
  calculate its outcome in the immediate pre-treatment period

- tname:

  name of column that holds the time period

- gname:

  name of column containing the unit's group

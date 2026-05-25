# get_Yibar_inner

Calculates a units average outcome across all periods. This function
operates on a data.frame that is already local to a particular unit.

## Usage

``` r
get_Yibar_inner(this_df, yname)
```

## Arguments

- this_df:

  a data.frame, for this function it should be specific to a particular
  unit

- yname:

  name of column containing the outcome (or other variable) for which to
  calculate its outcome in the immediate pre-treatment period

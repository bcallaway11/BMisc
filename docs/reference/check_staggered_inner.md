# check_staggered_inner

A helper function to check whether treatment is absorbing for a single
unit; that is, whether the unit never reverts from treated back to
untreated.

## Usage

``` r
check_staggered_inner(this_df, treatname, tname = NULL)
```

## Arguments

- this_df:

  a data.frame, for this function it should be specific to a particular
  unit

- treatname:

  name of column with the treatment indicator

- tname:

  name of column that holds the time period. If supplied, the unit's
  rows are sorted by it before checking. If \`NULL\` (the default), the
  rows are assumed to already be in time order.

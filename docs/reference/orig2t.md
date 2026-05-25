# orig2t

A helper function to switch from original time periods to "new" time
periods (which are just time periods going from 1 to total number of
available periods). This allows for periods not being exactly spaced
apart by 1.

## Usage

``` r
orig2t(orig, original_time.periods)
```

## Arguments

- orig:

  a vector of original time periods to convert to new time periods.

- original_time.periods:

  vector containing all original time periods.

## Value

new time period converted from original time period

## Examples

``` r
original_time.periods <- c(2001, 2003, 2005, 2007)
orig2t(c(2001, 2005), original_time.periods) ## returns c(1, 3)
#> [1] 1 3
```

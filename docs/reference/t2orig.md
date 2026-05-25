# t2orig

A helper function to switch from "new" t values to original t values.
This allows for periods not being exactly spaced apart by 1.

## Usage

``` r
t2orig(t, original_time.periods)
```

## Arguments

- t:

  a vector of time periods to convert back to original time periods.

- original_time.periods:

  vector containing all original time periods.

## Value

original time period converted from new time period

## Examples

``` r
original_time.periods <- c(2001, 2003, 2005, 2007)
t2orig(1:4, original_time.periods) ## returns c(2001, 2003, 2005, 2007)
#> [1] 2001 2003 2005 2007
```

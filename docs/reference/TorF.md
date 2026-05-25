# TorF

A function to replace NA's with FALSE in vector of logicals

## Usage

``` r
TorF(cond, use_isTRUE = FALSE)
```

## Arguments

- cond:

  a vector of conditions to check

- use_isTRUE:

  whether or not to use a vectorized version of isTRUE. This is
  generally slower but covers more cases.

## Value

logical vector

## Examples

``` r
TorF(c(TRUE, NA, FALSE)) ## NA becomes FALSE
#> [1]  TRUE FALSE FALSE
```

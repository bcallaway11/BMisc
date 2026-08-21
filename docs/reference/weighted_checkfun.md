# Weighted Check Function

Weights the check function

## Usage

``` r
weighted_checkfun(q, cvec, tau, weights)
```

## Arguments

- q:

  the value to check

- cvec:

  vector of data to compute quantiles for

- tau:

  between 0 and 1, ex. .5 implies get the median

- weights:

  the weights, weighted.checkfun normalizes the weights to sum to 1.

## Value

numeric

## Examples

``` r
x <- rnorm(100)
weighted_checkfun(0, x, tau = 0.5, weights = rep(1, 100))
#> [1] 0.3523878
```

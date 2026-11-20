# Quantiles of a Distribution Function

Quantiles of a distribution function created by
[`make_dist`](https://bcallaway11.github.io/BMisc/reference/make_dist.md)
(and so by
[`weighted_ecdf`](https://bcallaway11.github.io/BMisc/reference/weighted_ecdf.md)
and
[`combine_ecdfs`](https://bcallaway11.github.io/BMisc/reference/combine_ecdfs.md)),
computed by inverting it directly: for a step function, the smallest
support point \\y\\ with \\F(y) \ge p\\ (the generalized inverse,
`type = 1` in [`quantile`](https://rdrr.io/r/stats/quantile.html)); for
`method = "linear"`, the inverse of the piecewise-linear \\F\\.

Sample-quantile types other than 1 interpolate between order statistics
of a sample, which a weighted distribution function doesn't have, so
`type` is ignored (with a warning if it isn't 1).

## Usage

``` r
# S3 method for class 'BMisc_dist'
quantile(x, probs = seq(0, 1, 0.25), names = TRUE, ...)
```

## Arguments

- x:

  a distribution function from
  [`make_dist`](https://bcallaway11.github.io/BMisc/reference/make_dist.md).

- probs:

  numeric vector of probabilities in \\\[0, 1\]\\.

- names:

  logical; if `TRUE`, name the result by `probs`, as
  [`quantile`](https://rdrr.io/r/stats/quantile.html) does.

- ...:

  ignored, except `type` (see Description).

## Value

numeric vector of quantiles.

## Examples

``` r
F <- weighted_ecdf(c(0, 1), weights = c(0.4, 0.6))
quantile(F, 0.5) # 1
#> 50% 
#>   1 
```

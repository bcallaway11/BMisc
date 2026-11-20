# Combine Distribution Functions

Combines distribution functions into their mixture \\F(y) = \sum_c w_c
F_c(y)\\, with weights given by `weights`.

By default (`y.seq = NULL`) the mixture is exact: when every
distribution function is a step function (as from
[`stats::ecdf()`](https://rdrr.io/r/stats/ecdf.html),
[`weighted_ecdf()`](https://bcallaway11.github.io/BMisc/reference/weighted_ecdf.md)
or
[`make_dist()`](https://bcallaway11.github.io/BMisc/reference/make_dist.md)),
so is the mixture, with a jump at each support point \\y\_{cj}\\ of each
\\F_c\\ equal to \\w_c\\ times the jump of \\F_c\\ there. These are
stacked and sorted, and their cumulative sums give the mixture at every
support point. [`quantile()`](https://rdrr.io/r/stats/quantile.html) of
the result is then exact as well.

With `y.seq`, the mixture is instead evaluated only at the points
`y.seq` (as in BMisc 1.4.10 and earlier), and its quantiles are rounded
to those points; this also works for distribution functions that are not
step functions.

## Usage

``` r
combine_ecdfs(y.seq = NULL, ecdflist, weights = NULL, ...)
```

## Arguments

- y.seq:

  `NULL` (the default) for the exact mixture, or a sequence of y values
  at which to evaluate it

- ecdflist:

  list of ecdf objects (distribution functions) to combine

- weights:

  a vector of weights to put on each distribution function, which should
  sum to 1; if weights are not provided then equal weight is given to
  each distribution function. Distribution functions with weight 0 are
  skipped in the exact mixture.

- ...:

  additional arguments that can be past to BMisc::make_dist

## Value

ecdf (a `BMisc_dist` object, see
[`make_dist()`](https://bcallaway11.github.io/BMisc/reference/make_dist.md))

## Examples

``` r
x <- rnorm(100)
y <- rnorm(100, 1, 1)
Fx <- ecdf(x)
Fy <- ecdf(y)
both <- combine_ecdfs(ecdflist = list(Fx, Fy))
quantile(both, 0.5) # exact median of the mixture
#>       50% 
#> 0.4642586 
plot(Fx, col = "green")
plot(Fy, col = "blue", add = TRUE)
plot(both, add = TRUE)


# evaluated on a grid only
both_grid <- combine_ecdfs(seq(-2, 3, 0.1), list(Fx, Fy))
```

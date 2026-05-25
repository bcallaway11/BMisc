# Make a Distribution Function

turn vectors of a values and their distribution function values into an
ecdf. Vectors should be the same length and both increasing.

## Usage

``` r
make_dist(
  x,
  Fx,
  sorted = FALSE,
  rearrange = FALSE,
  force01 = FALSE,
  method = "constant"
)
```

## Arguments

- x:

  vector of values

- Fx:

  vector of the distribution function values

- sorted:

  boolean indicating whether or not x is already sorted; computation is
  somewhat faster if already sorted

- rearrange:

  boolean indicating whether or not should monotize distribution
  function

- force01:

  boolean indicating whether or not to force the values of the
  distribution function (i.e. Fx) to be between 0 and 1

- method:

  which method to pass to `approxfun` to approximate the distribution
  function. Default is "constant"; other possible choice is "linear".
  "constant" returns a step function, just like an empirical cdf;
  "linear" linearly interpolates between neighboring points.

## Value

ecdf

## Examples

``` r
y <- rnorm(100)
y <- y[order(y)]
u <- runif(100)
u <- u[order(u)]
F <- make_dist(y, u)
```

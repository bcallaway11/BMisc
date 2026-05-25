# Combine Two Distribution Functions

Combines two distribution functions with given weights by \`weights\`

## Usage

``` r
combine_ecdfs(y.seq, ecdflist, weights = NULL, ...)
```

## Arguments

- y.seq:

  sequence of possible y values

- ecdflist:

  list of ecdf objects (distribution functions) to combine

- weights:

  a vector of weights to put on each distribution function; if weights
  are not provided then equal weight is given to each distribution
  function

- ...:

  additional arguments that can be past to BMisc::make_dist

## Value

ecdf

## Examples

``` r
x <- rnorm(100)
y <- rnorm(100, 1, 1)
Fx <- ecdf(x)
Fy <- ecdf(y)
both <- combine_ecdfs(seq(-2, 3, 0.1), list(Fx, Fy))
plot(Fx, col = "green")
plot(Fy, col = "blue", add = TRUE)
plot(both, add = TRUE)

```

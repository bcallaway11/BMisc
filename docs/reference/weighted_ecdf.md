# Weighted Distribution Function

Get a distribution function from a vector of values after applying some
weights

## Usage

``` r
weighted_ecdf(y, y.seq = NULL, weights = NULL, norm = TRUE)
```

## Arguments

- y:

  a vector to compute the mean for

- y.seq:

  an optional vector of values to compute the distribution function for;
  the default is to use all unique values of y

- weights:

  the vector of weights, can be NULL, then will just return mean

- norm:

  normalize the weights so that they have mean of 1, default is to
  normalize

## Value

ecdf

## Examples

``` r
y <- rnorm(100)
w <- runif(100)
F <- weighted_ecdf(y, weights = w)
F(0) ## approx 0.5
#> [1] 0.3827229
```

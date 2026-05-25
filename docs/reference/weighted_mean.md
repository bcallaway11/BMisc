# Weighted Mean

Get the mean applying some weights

## Usage

``` r
weighted_mean(y, weights = NULL, norm = TRUE)
```

## Arguments

- y:

  a vector to compute the mean for

- weights:

  the vector of weights, can be NULL, then will just return mean

- norm:

  normalize the weights so that they have mean of 1, default is to
  normalize

## Value

the weighted mean

## Examples

``` r
y <- rnorm(100)
w <- runif(100)
weighted_mean(y, weights = w)
#> [1] -0.06622311
```

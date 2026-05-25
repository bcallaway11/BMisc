# weighted_quantile

function to recover quantiles of a vector with weights

## Usage

``` r
weighted_quantile(tau, cvec, weights = NULL, norm = TRUE)
```

## Arguments

- tau:

  a vector of values between 0 and 1

- cvec:

  a vector to compute quantiles for

- weights:

  the weights, weighted.checkfun normalizes the weights to sum to 1.

- norm:

  normalize the weights so that they have mean of 1, default is to
  normalize

## Value

vector of quantiles

## Examples

``` r
y <- rnorm(100)
w <- runif(100)
weighted_quantile(c(0.25, 0.5, 0.75), y, weights = w)
#> [1] -0.5317279  0.1819952  0.7973391
```

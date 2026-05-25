# Quantile of a Weighted Check Function

Finds the quantile by optimizing the weighted check function

## Usage

``` r
weighted_quantile_inner(tau, cvec, weights = NULL, norm = TRUE)
```

## Arguments

- tau:

  between 0 and 1, ex. .5 implies get the median

- cvec:

  a vector to compute quantiles for

- weights:

  the weights, weighted.checkfun normalizes the weights to sum to 1.

- norm:

  normalize the weights so that they have mean of 1, default is to
  normalize

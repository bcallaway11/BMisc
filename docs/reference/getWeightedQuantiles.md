# getWeightedQuantiles

Legacy version of \`weighted_quantile\`, please use that function
instead. This function will eventually be deleted.

## Usage

``` r
getWeightedQuantiles(tau, cvec, weights = NULL, norm = TRUE)
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

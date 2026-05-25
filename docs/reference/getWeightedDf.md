# getWeightedDf

Legacy version of \`weighted_ecdf\`, please use that function instead.
This function will eventually be deleted.

## Usage

``` r
getWeightedDf(y, y.seq = NULL, weights = NULL, norm = TRUE)
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

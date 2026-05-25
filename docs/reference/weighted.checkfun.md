# weighted.checkfun

Legacy version of \`weighted_checkfun\`, please use that function
instead. This function will eventually be deleted.

## Usage

``` r
weighted.checkfun(q, cvec, tau, weights)
```

## Arguments

- q:

  the value to check

- cvec:

  vector of data to compute quantiles for

- tau:

  between 0 and 1, ex. .5 implies get the median

- weights:

  the weights, weighted.checkfun normalizes the weights to sum to 1.

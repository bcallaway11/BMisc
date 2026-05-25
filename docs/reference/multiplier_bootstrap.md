# multiplier_bootstrap

A function that takes in an influence function (an nxk matrix) and the
number of bootstrap iterations and returns a Bxk matrix of bootstrap
results. This function uses Rademechar weights.

## Usage

``` r
multiplier_bootstrap(inf_func, biters)
```

## Arguments

- inf_func:

  nxk matrix of (e.g., these could be a matrix containing the influence
  function for different parameter estimates)

- biters:

  the number of bootstrap iterations

## Value

a Bxk matrix

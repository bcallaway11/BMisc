# element_wise_mult

This is a function that takes in two matrices of dimension nxB and nxk
and returns a Bxk matrix that comes from element-wise multiplication of
every column in the first matrix times the entire second matrix and the
averaging over the n-dimension. It is equivalent (but faster than) the
following R code: \`sapply(1:biters, function(b)
sqrt(n)\*colMeans(Umat\[,b\]\*inf.func))\` . This function is
particularly useful for fast computations using the multiplier
bootstrap.

## Usage

``` r
element_wise_mult(U, inf_func)
```

## Arguments

- U:

  nxB matrix (e.g., these could be a matrix of Rademachar weights for B
  bootstrap iterations using the multiplier bootstrap

- inf_func:

  nxk matrix of (e.g., these could be a matrix containing the influence
  function for different parameter estimates)

## Value

a Bxk matrix

# multiplier_bootstrap

A function that takes in an influence function (an nxk matrix) and the
number of bootstrap iterations and returns a Bxk matrix of bootstrap
results. This function uses Rademacher weights.

## Usage

``` r
multiplier_bootstrap(inf_func, biters, method = c("auto", "gemm", "loop"))
```

## Arguments

- inf_func:

  nxk matrix of (e.g., these could be a matrix containing the influence
  function for different parameter estimates)

- biters:

  the number of bootstrap iterations

- method:

  one of `"auto"` (the default), `"gemm"` to always use the blocked
  matrix multiplication, or `"loop"` to always accumulate one draw at a
  time.

## Value

a Bxk matrix

## Details

The influence function is read once, in blocks of rows, and each block
is combined with all `biters` draws at once. With an optimized BLAS
(Accelerate, OpenBLAS, MKL, BLIS) that is a single matrix multiplication
per block, which is much faster. With R's reference BLAS it is not, and
the draws are accumulated one at a time within each block instead.
`method = "auto"` picks between the two from
`extSoftVersion()[["BLAS"]]`. Both give the same draws for a given seed
(up to floating-point rounding), so the choice only affects speed.

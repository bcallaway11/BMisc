# drop_collinear

A function to check for multicollinearity and drop collinear terms from
a matrix

## Usage

``` r
drop_collinear(matrix)
```

## Arguments

- matrix:

  a matrix for which the function will remove collinear columns

## Value

a matrix with collinear columns removed

## Examples

``` r
# \donttest{
  if (requireNamespace("caret", quietly = TRUE)) {
    X <- cbind(1:5, 2 * (1:5), rnorm(5))
    colnames(X) <- c("x1", "x2", "x3")
    drop_collinear(X) ## x2 dropped as collinear with x1
  }
#> Warning: The following covariates were dropped due to collinearity: x2
#>      x1         x3
#> [1,]  1  0.4474135
#> [2,]  2 -0.4545706
#> [3,]  3 -0.0605266
#> [4,]  4 -0.5895564
#> [5,]  5  0.2506474
# }
```

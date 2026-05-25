# Matrix-Vector Multiplication

This function multiplies a matrix by a vector and returns a numeric
vector.

## Usage

``` r
mv_mult(A, v)
```

## Arguments

- A:

  an nxk matrix.

- v:

  a vector (can be stored as numeric or as a kx1 matrix)

## Value

A numeric vector resulting from the multiplication of the matrix by the
vector.

## Examples

``` r
A <- matrix(1:9, nrow = 3, ncol = 3)
v <- c(2, 4, 6)
mv_mult(A, v)
#> [1] 60 72 84
```

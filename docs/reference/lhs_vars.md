# Left-hand Side Variables

Take a formula and return a vector of the variables on the left hand
side, it will return NULL for a one sided formula

## Usage

``` r
lhs_vars(formula)
```

## Arguments

- formula:

  a formula

## Value

vector of variable names

## Examples

``` r
ff <- yvar ~ x1 + x2
lhs_vars(ff)
#> [1] "yvar"
```

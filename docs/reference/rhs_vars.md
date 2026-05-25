# Right-hand Side Variables

Take a formula and return a vector of the variables on the right hand
side

## Usage

``` r
rhs_vars(formula)
```

## Arguments

- formula:

  a formula

## Value

vector of variable names

## Examples

``` r
ff <- yvar ~ x1 + x2
rhs_vars(ff)
#> [1] "x1" "x2"

ff <- y ~ x1 + I(x1^2)
rhs_vars(ff)
#> [1] "x1"      "I(x1^2)"
```

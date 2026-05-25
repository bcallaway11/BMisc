# Right-hand Side of Formula

Take a formula and return the right hand side of the formula

## Usage

``` r
rhs(formula)
```

## Arguments

- formula:

  a formula

## Value

a one sided formula

## Examples

``` r
ff <- yvar ~ x1 + x2
rhs(ff)
#> Warning: 'rhs.vars' is deprecated.
#> Use 'rhs_vars' instead.
#> See help("Deprecated")
#> ~x1 + x2
#> <environment: 0x583ac29a40f0>
```

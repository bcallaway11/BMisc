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
#> ~x1 + x2
#> <environment: 0x574a17701bf0>
```

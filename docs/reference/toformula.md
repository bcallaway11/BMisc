# Variable Names to Formula

take a name for a y variable and a vector of names for x variables and
turn them into a formula

## Usage

``` r
toformula(yname, xnames)
```

## Arguments

- yname:

  the name of the y variable

- xnames:

  vector of names for x variables

## Value

a formula

## Examples

``` r
toformula("yvar", c("x1", "x2"))
#> yvar ~ x1 + x2
#> <environment: 0x6519f8e99b10>

## should return yvar ~ 1
toformula("yvar", rhs_vars(~1))
#> yvar ~ 1
#> <environment: 0x6519f8ee1bd8>
```

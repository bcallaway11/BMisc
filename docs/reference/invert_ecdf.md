# Invert Ecdf

take an ecdf object and invert it to get a step-quantile function

## Usage

``` r
invert_ecdf(df)
```

## Arguments

- df:

  an ecdf object

## Value

stepfun object that contains the quantiles of the df

## Examples

``` r
y <- rnorm(100)
F <- ecdf(y)
Finv <- invert_ecdf(F)
Finv(0.5) ## approximate median
#> [1] -0.06007258
```

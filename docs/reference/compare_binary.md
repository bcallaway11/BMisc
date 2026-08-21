# Compare Variables across Groups

`compare_binary` takes in a variable e.g. union and runs bivariate
regression of x on treatment (for summary statistics)

## Usage

``` r
compare_binary(
  x,
  on,
  dta,
  w = rep(1, nrow(dta)),
  report = c("diff", "levels", "both")
)
```

## Arguments

- x:

  variables to run regression on

- on:

  binary variable

- dta:

  the data to use

- w:

  weights

- report:

  which type of report to make; diff is the difference between the two
  variables by group

## Value

matrix of results

## Examples

``` r
dta <- data.frame(x = rnorm(100), treat = rep(c(0, 1), 50))
compare_binary("x", "treat", dta, report = "diff")
#> [1] 0.09063227 0.11632150 0.00000000
```

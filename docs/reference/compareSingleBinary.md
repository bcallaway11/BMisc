# Compare a single variable across two groups

`compareBinary` takes in a variable e.g. union and runs bivariate
regression of x on treatment (for summary statistics)

## Usage

``` r
compareSingleBinary(
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

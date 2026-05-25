# compareBinary

Legacy version of \`compare_binary\`, please use that function instead.
This function will eventually be deleted.

## Usage

``` r
compareBinary(
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

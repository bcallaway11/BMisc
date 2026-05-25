# get_lagYi

A function that calculates lagged outcomes in a panel data setting. If
the data.frame that is passed in has nxT rows, the resulting vector will
also have nxT elements with one element for each unit set to be NA

## Usage

``` r
get_lagYi(df, idname, yname, tname, nlags = 1)
```

## Arguments

- df:

  the data.frame used in the function

- idname:

  name of column that holds the unit id

- yname:

  name of column containing the outcome (or other variable) for which to
  calculate its outcome in the immediate pre-treatment period

- tname:

  name of column that holds the time period

- nlags:

  The number of periods to lag. The default is 1, which computes the lag
  from the previous period.

## Examples

``` r
n <- 50
id <- rep(seq_len(n), each = 4)
t <- rep(1:4, n)
y <- rnorm(n * 4)
dta <- data.frame(id = id, t = t, y = y)
dta$lag_y <- get_lagYi(dta, idname = "id", yname = "y", tname = "t")
```

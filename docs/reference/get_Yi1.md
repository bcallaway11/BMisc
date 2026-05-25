# get_Yi1

A function to calculate outcomes for units in the first time period that
is available in a panel data setting (this function can also be used to
recover covariates, etc. in the first period).

## Usage

``` r
get_Yi1(df, idname, yname, tname, gname)
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

- gname:

  name of column containing the unit's group

## Examples

``` r
n <- 50
id <- rep(seq_len(n), each = 4)
t <- rep(1:4, n)
y <- rnorm(n * 4)
g <- rep(sample(c(0, 2, 3), n, replace = TRUE), each = 4)
dta <- data.frame(id = id, t = t, y = y, group = g)
dta$Yi1 <- get_Yi1(dta, idname = "id", yname = "y", tname = "t", gname = "group")
```

# get_Yibar_pre

A function to calculate average outcomes for units in their
pre-treatment periods (this function can also be used to recover
pre-treatment averages of covariates, etc.). For units that do not
participate in the treatment (and therefore have group==0), the function
calculates their overall average outcome.

## Usage

``` r
get_Yibar_pre(df, idname, yname, tname, gname)
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
dta$Yibarpre <- get_Yibar_pre(dta, idname = "id", yname = "y",
                               tname = "t", gname = "group")
```

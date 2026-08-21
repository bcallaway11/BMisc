# get_YiGmin1

A function to calculate outcomes for units in the period right before
they become treated (this function can also be used to recover
covariates, etc. in the period right before a unit becomes treated). For
units that do not participate in the treatment (and therefore have
group==0), they are assigned their outcome in the last period.

## Usage

``` r
get_YiGmin1(df, idname, yname, tname, gname)
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
dta$YiGmin1 <- get_YiGmin1(dta, idname = "id", yname = "y",
                            tname = "t", gname = "group")
head(unique(dta[, c("id", "group", "YiGmin1")]))
#>    id group     YiGmin1
#> 1   1     0  0.20159751
#> 5   2     0  0.96381390
#> 9   3     3  1.07086109
#> 13  4     3 -0.27971976
#> 17  5     2 -0.74916542
#> 21  6     3 -0.02899503
```

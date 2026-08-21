# get_Yit

A function to calculate outcomes for units in a particular time period
\`tp\` in a panel data setting (this function can also be used to
recover covariates, etc. in the first period).

## Usage

``` r
get_Yit(df, tp, idname, yname, tname)
```

## Arguments

- df:

  the data.frame used in the function

- tp:

  The time period for which to get the outcome

- idname:

  name of column that holds the unit id

- yname:

  name of column containing the outcome (or other variable) for which to
  calculate its outcome in the immediate pre-treatment period

- tname:

  name of column that holds the time period

## Value

a vector of outcomes in period t, the vector will have the length nT
(i.e., this is returned for each element in the panel, not for a
particular period); \`NA\` for units not observed in period \`tp\`

## Examples

``` r
n <- 50
id <- rep(seq_len(n), each = 4)
t <- rep(1:4, n)
y <- rnorm(n * 4)
dta <- data.frame(id = id, t = t, y = y)
Yit2 <- get_Yit(dta, tp = 2, idname = "id", yname = "y", tname = "t")
length(Yit2) ## n * 4
#> [1] 200
```

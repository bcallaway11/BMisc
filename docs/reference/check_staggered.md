# check_staggered

A function to check if treatment is staggered in a panel data set.

## Usage

``` r
check_staggered(df, idname, treatname)
```

## Arguments

- df:

  the data.frame used in the function

- idname:

  name of column that holds the unit id

- treatname:

  name of column with the treatment indicator

## Value

a logical indicating whether treatment is staggered

## Examples

``` r
n <- 50
id <- rep(seq_len(n), each = 4)
t <- rep(1:4, n)
g <- rep(sample(c(0, 2, 3), n, replace = TRUE), each = 4)
treat <- as.integer(t >= g & g > 0)
dta <- data.frame(id = id, t = t, treat = treat)
check_staggered(dta, idname = "id", treatname = "treat")
#> [1] FALSE
```

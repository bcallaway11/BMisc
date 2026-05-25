# get_group

A function to calculate a unit's group in a panel data setting with a
binary treatment and staggered treatment adoption and where there is a
column in the data indicating whether or not a unit is treated

## Usage

``` r
get_group(df, idname, tname, treatname)
```

## Arguments

- df:

  the data.frame used in the function

- idname:

  name of column that holds the unit id

- tname:

  name of column that holds the time period

- treatname:

  name of column with the treatment indicator

## Examples

``` r
n <- 50
id <- rep(seq_len(n), each = 4)
t <- rep(1:4, n)
g <- rep(sample(c(0, 2, 3), n, replace = TRUE), each = 4)
treat <- as.integer(t >= g & g > 0)
dta <- data.frame(id = id, t = t, treat = treat)
dta$group <- get_group(dta, idname = "id", tname = "t", treatname = "treat")
head(unique(dta[, c("id", "group")]))
#>    id group
#> 1   1     2
#> 5   2     2
#> 9   3     2
#> 13  4     2
#> 17  5     0
#> 21  6     2
```

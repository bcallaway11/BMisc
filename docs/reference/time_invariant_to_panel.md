# time_invariant_to_panel

This function takes a time-invariant variable and repeats it for each
period in a panel data set.

## Usage

``` r
time_invariant_to_panel(x, df, idname, balanced_panel = TRUE)
```

## Arguments

- x:

  a vector of length equal to the number of unique ids in df.

- df:

  the data.frame used in the function

- idname:

  name of column that holds the unit id

- balanced_panel:

  a logical indicating whether the panel is balanced. If TRUE, the
  function will optimize the repetition process. Default is TRUE.

## Value

a vector of length equal to the number of rows in df.

## Examples

``` r
n <- 50
id <- rep(seq_len(n), each = 4)
t <- rep(1:4, n)
dta <- data.frame(id = id, t = t)
x_unit <- rnorm(n)
x_panel <- time_invariant_to_panel(x_unit, dta, idname = "id")
length(x_panel) ## n * 4
#> [1] 200
```

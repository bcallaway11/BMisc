# Balance a Panel Data Set

This function drops observations from data.frame that are not part of
balanced panel data set.

## Usage

``` r
make_balanced_panel(data, idname, tname, return_data.table = FALSE)
```

## Arguments

- data:

  data.frame used in function

- idname:

  unique id

- tname:

  time period name

- return_data.table:

  if TRUE, make_balanced_panel will return a data.table rather than a
  data.frame. Default is FALSE.

## Value

data.frame that is a balanced panel

## Examples

``` r
id <- rep(seq(1, 100), each = 2) # individual ids for setting up a two period panel
t <- rep(seq(1, 2), 100) # time periods
y <- rnorm(200) # outcomes
dta <- data.frame(id = id, t = t, y = y) # make into data frame
dta <- dta[-7, ] # drop the 7th row from the dataset (which creates an unbalanced panel)
dta <- make_balanced_panel(dta, idname = "id", tname = "t")
```

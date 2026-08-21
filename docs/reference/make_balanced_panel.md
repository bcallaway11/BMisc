# Balance a Panel Data Set

This function drops observations from data.frame that are not part of
balanced panel data set. \`data\` is copied before dropping any rows, so
the object passed in is left unmodified; see
[`set_balanced_panel`](https://bcallaway11.github.io/BMisc/reference/set_balanced_panel.md)
for an in-place alternative that avoids copying large inputs.

## Usage

``` r
make_balanced_panel(data, idname, tname)
```

## Arguments

- data:

  data.frame (or data.table) used in function

- idname:

  unique id

- tname:

  time period name

## Value

a balanced panel, with the same class (\`data.frame\` or \`data.table\`)
as \`data\`

## Examples

``` r
id <- rep(seq(1, 100), each = 2) # individual ids for setting up a two period panel
t <- rep(seq(1, 2), 100) # time periods
y <- rnorm(200) # outcomes
dta <- data.frame(id = id, t = t, y = y) # make into data frame
dta <- dta[-7, ] # drop the 7th row from the dataset (which creates an unbalanced panel)
dta <- make_balanced_panel(dta, idname = "id", tname = "t")
```

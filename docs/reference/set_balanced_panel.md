# set_balanced_panel

In-place version of
[`make_balanced_panel`](https://bcallaway11.github.io/BMisc/reference/make_balanced_panel.md)
for large panels where copying \`data\` is undesirable. Converts
\`data\` to a data.table by reference (mutating the caller's object into
a data.table as a side effect, if it is not one already) instead of
copying it. The balanced result must still be captured from the return
value, the same as any other function in this package (filtering out
unbalanced units cannot itself happen without allocating the smaller,
filtered result somewhere).

## Usage

``` r
set_balanced_panel(data, idname, tname)
```

## Arguments

- data:

  data.frame (or data.table) used in function

- idname:

  unique id

- tname:

  time period name

## Value

a balanced data.table

## Examples

``` r
id <- rep(seq(1, 100), each = 2)
t <- rep(seq(1, 2), 100)
y <- rnorm(200)
dta <- data.frame(id = id, t = t, y = y)
dta <- dta[-7, ]
dta <- set_balanced_panel(dta, idname = "id", tname = "t") # dta is now a data.table
```

# sort_panel

Sorts a panel data set by unit id and then time period. \`data\` is
copied before sorting, so the object passed in is left unmodified. For
large panels where copying is undesirable, sort in place instead with
\`data.table::setorderv(data, c(idname, tname))\`.

## Usage

``` r
sort_panel(data, idname, tname)
```

## Arguments

- data:

  data.frame (or data.table) used in function

- idname:

  unique id

- tname:

  time period name

## Value

\`data\` sorted by \`(idname, tname)\`, with the same class
(\`data.frame\` or \`data.table\`) as \`data\`

## Examples

``` r
id <- rep(sample(1:5), each = 2) # units in a shuffled, unsorted order
t <- rep(c(2, 1), 5) # each unit's own periods out of time order
dta <- data.frame(id = id, t = t, y = rnorm(10))
dta <- sort_panel(dta, idname = "id", tname = "t")
```

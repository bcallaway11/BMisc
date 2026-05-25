# Panel Data to Repeated Cross Sections

panel2cs2 takes a 2 period dataset and turns it into a cross sectional
dataset; i.e., long to wide. This function considers a particular case
where there is some outcome whose value can change over time. It returns
the dataset from the first period with the outcome in the second period
and the change in outcomes over time appended to it

## Usage

``` r
panel2cs2(data, yname, idname, tname, balance_panel = TRUE)
```

## Arguments

- data:

  data.frame used in function

- yname:

  name of outcome variable that can change over time

- idname:

  unique id

- tname:

  time period name

- balance_panel:

  whether to ensure that panel is balanced. Default is TRUE, but code
  runs somewhat faster if this is set to be FALSE.

## Value

data from first period with .y0 (outcome in first period), .y1 (outcome
in second period), and .dy (change in outcomes over time) appended to it

## Examples

``` r
id <- rep(seq(1, 50), 2)
t <- rep(seq(1, 2), each = 50)
y <- rnorm(100)
dta <- data.frame(id = id, t = t, y = y)
out <- panel2cs2(dta, yname = "y", idname = "id", tname = "t")
head(out[, c("id", ".y0", ".y1", ".dy")])
#>       id        .y0         .y1        .dy
#>    <int>      <num>       <num>      <num>
#> 1:     1 -1.0637046  0.41212408  1.4758286
#> 2:     2 -0.1941525  0.59570991  0.7898624
#> 3:     3 -0.4965192 -0.08125807  0.4152611
#> 4:     4  1.6513326  0.88126900 -0.7700636
#> 5:     5  1.0458520  0.94490127 -0.1009508
#> 6:     6  0.5729193  2.37091477  1.7979955
```

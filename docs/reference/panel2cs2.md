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
#>       id        .y0        .y1        .dy
#>    <int>      <num>      <num>      <num>
#> 1:     1 -0.7611721  2.5955239  3.3566960
#> 2:     2  0.8295126  0.4192840 -0.4102286
#> 3:     3 -1.0551720 -0.2816984  0.7734735
#> 4:     4  0.2229680  0.5448631  0.3218952
#> 5:     5  2.4411950  0.8682307 -1.5729643
#> 6:     6  2.0006417 -0.2066753 -2.2073170
```

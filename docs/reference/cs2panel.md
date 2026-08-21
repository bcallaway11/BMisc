# Cross Section to Panel

Turn repeated cross sections data into panel data by imposing rank
invariance; does not require that the inputs have the same length

## Usage

``` r
cs2panel(cs1, cs2, yname)
```

## Arguments

- cs1:

  data frame, the first cross section

- cs2:

  data frame, the second cross section

- yname:

  the name of the variable to calculate difference for (should be the
  same in each dataset)

## Value

the change in outcomes over time

## Examples

``` r
cs1 <- data.frame(y = rnorm(100))
cs2 <- data.frame(y = rnorm(100, mean = 1))
dy <- cs2panel(cs1, cs2, "y")
mean(dy) ## approx 1
#> [1] 1.05836
```

# Check Function

The check function used for optimizing to get quantiles

## Usage

``` r
checkfun(a, tau)
```

## Arguments

- a:

  vector to compute quantiles for

- tau:

  between 0 and 1, ex. .5 implies get the median

## Value

numeric value

## Examples

``` r
x <- rnorm(100)
x[which.min(checkfun(x, 0.5))] ## should be around 0
#> [1] 0.01005771
```

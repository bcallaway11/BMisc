# Drop a Covariate from a Formula

`drop_cov_from_formula` adds drops some covariates from a formula; covs
should be a list of variable names

## Usage

``` r
drop_cov_from_formula(covs, formula)
```

## Arguments

- covs:

  should be a list of variable names

- formula:

  the formula to drop covariates from

## Value

formula

## Examples

``` r
ff <- y ~ x + w + z
drop_cov_from_formula(list("w", "z"), ff)
#> y ~ x
#> <environment: 0x64576f2552d8>

drop_cov_from_formula("z", ff)
#> y ~ x + w
#> <environment: 0x64576f1d4758>
```

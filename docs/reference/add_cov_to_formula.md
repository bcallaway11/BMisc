# Add a Covariate to a Formula

`add_cov_to_formula` adds some covariates to a formula; covs should be a
list of variable names

## Usage

``` r
add_cov_to_formula(covs, formula)
```

## Arguments

- covs:

  should be a list of variable names

- formula:

  which formula to add covariates to

## Value

formula

## Examples

``` r
ff <- y ~ x
add_cov_to_formula(list("w", "z"), ff)
#> y ~ x + w + z
#> <environment: 0x583aafede640>

ff <- ~x
add_cov_to_formula("z", ff)
#> ~x + z
#> <environment: 0x583ab2ec4800>
```

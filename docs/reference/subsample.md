# Subsample of Observations from Panel Data

returns a subsample of a panel data set; in particular drops all
observations that are not in `keepids`. If it is not set, randomly keeps
`nkeep` observations.

## Usage

``` r
subsample(dta, idname, tname, keepids = NULL, nkeep = NULL)
```

## Arguments

- dta:

  a data.frame which is a balanced panel

- idname:

  the name of the id variable

- tname:

  the name of the time variable

- keepids:

  which ids to keep

- nkeep:

  how many ids to keep (only used if `keepids` is not set); the default
  is the number of unique ids

## Value

a data.frame that contains a subsample of `dta`

## Examples

``` r
data("LaborSupply", package = "plm")
nrow(LaborSupply)
#> [1] 5320
unique(LaborSupply$year)
#>  [1] 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988
ss <- subsample(LaborSupply, "id", "year", nkeep = 100)
nrow(ss)
#> [1] 1000
```

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
#> Loading required namespace: plm
#> Error in DONTSHOW({    if (!requireNamespace("plm")) {        if (interactive() || is.na(Sys.getenv("_R_CHECK_PACKAGE_NAME_",             NA))) {            stop("package 'plm' is required for this example")        }        else {            q()        }    }}): package 'plm' is required for this example
data("LaborSupply", package = "plm")
#> Error in find.package(package, lib.loc, verbose = verbose): there is no package called ‘plm’
nrow(LaborSupply)
#> Error: object 'LaborSupply' not found
unique(LaborSupply$year)
#> Error: object 'LaborSupply' not found
ss <- subsample(LaborSupply, "id", "year", nkeep = 100)
#> Error: object 'LaborSupply' not found
nrow(ss)
#> Error: object 'ss' not found
```

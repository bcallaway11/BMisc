# Block Bootstrap

make draws of all observations with the same id in a panel data context.
This is useful for bootstrapping with panel data.

## Usage

``` r
block_boot_sample(data, idname)
```

## Arguments

- data:

  data.frame from which you want to bootstrap

- idname:

  column in data which contains an individual identifier

## Value

data.frame bootstrapped from the original dataset; this data.frame will
contain new ids

## Examples

``` r
#> Loading required namespace: plm
#> Error in DONTSHOW({    if (!requireNamespace("plm")) {        if (interactive() || is.na(Sys.getenv("_R_CHECK_PACKAGE_NAME_",             NA))) {            stop("package 'plm' is required for this example")        }        else {            q()        }    }}): package 'plm' is required for this example
data("LaborSupply", package = "plm")
#> Error in find.package(package, lib.loc, verbose = verbose): there is no package called ‘plm’
bbs <- block_boot_sample(LaborSupply, "id")
#> Error: object 'LaborSupply' not found
nrow(bbs)
#> Error: object 'bbs' not found
head(bbs$id)
#> Error: object 'bbs' not found
```

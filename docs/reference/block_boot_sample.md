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
data("LaborSupply", package = "plm")
bbs <- block_boot_sample(LaborSupply, "id")
nrow(bbs)
#> [1] 5320
head(bbs$id)
#> [1] 1 1 1 1 1 1
```

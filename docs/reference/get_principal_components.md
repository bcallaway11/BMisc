# get_principal_components

A function to calculate unit-specific principal components, given panel
data

## Usage

``` r
get_principal_components(
  xformula,
  data,
  idname,
  tname,
  n_components = NULL,
  ret_wide = FALSE,
  ret_id = FALSE
)
```

## Arguments

- xformula:

  a formula specifying the variables to use in the principal component
  analysis

- data:

  a data.frame containing the panel data

- idname:

  the name of the column containing the unit id

- tname:

  the name of the column containing the time period

- n_components:

  the number of principal components to retain, the default is NULL
  which will result in all principal components being retained

- ret_wide:

  whether to return the data in wide format (where the number of rows is
  equal to n = length(unique(data\[\[idname\]\])) or long format (where
  the number of rows is equal to nT = nrow(data)). The default is FALSE,
  so that long data is returned by default.

- ret_id:

  whether to return the id column in the output data.frame. The default
  is FALSE.

## Value

a data.frame containing the original data with the principal components
appended

## Examples

``` r
n <- 20
id <- rep(seq_len(n), each = 4)
t <- rep(1:4, n)
x1 <- rnorm(n * 4)
dta <- data.frame(id = id, t = t, x1 = x1)
pcs <- get_principal_components(~x1, dta, idname = "id", tname = "t")
dim(pcs)
#> [1] 80  4
```

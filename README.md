

<!-- README.md is generated from README.qmd. Please edit that file -->

# BMisc <img src="man/figures/logo.png" align="right" height="139" alt="" />

[![CRAN downloads
(total)](http://cranlogs.r-pkg.org/badges/grand-total/BMisc?color=blue.png)](https://cran.r-project.org/package=BMisc)
[![CRAN downloads
(monthly)](http://cranlogs.r-pkg.org/badges/last-month/BMisc?color=blue.png)](https://cran.r-project.org/package=BMisc)
[![CRAN
version](https://www.r-pkg.org/badges/version/BMisc?color=blue.png)](https://cran.r-project.org/package=BMisc)
[![Development
version](https://img.shields.io/badge/devel%20version-1.4.10-blue.svg)](https://github.com/bcallaway11/BMisc)
[![CRAN
checks](https://badges.cranchecks.info/summary/BMisc.svg)](https://cran.r-project.org/web/checks/check_results_BMisc.html)
[![Last
commit](https://img.shields.io/github/last-commit/bcallaway11/BMisc.svg)](https://github.com/bcallaway11/BMisc/commits/master)

BMisc includes miscellaneous functions useful for applied econometrics,
with a focus on panel data and distributional analysis. Utilities cover
balancing panels, working with distribution functions, computing
weighted statistics, manipulating formulas, and supporting staggered
treatment adoption settings (e.g., identifying treatment groups and
recovering pre-treatment outcomes).

## Installation

You can install BMisc from CRAN with:

``` r
install.packages("BMisc")
```

Or install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("bcallaway11/BMisc")
```

## Example 1: Working with distribution functions

`make_dist` creates a distribution function from a vector of values and
their corresponding CDF values.

``` r
library(BMisc)
y <- rnorm(100)
y <- y[order(y)]
u <- runif(100)
u <- u[order(u)]
F <- make_dist(y, u)
class(F)
#> [1] "ecdf"     "stepfun"  "function"
# plot(F)
```

## Example 2: Working with panel data

`make_balanced_panel` drops observations from a panel dataset that are
not available in all time periods.

``` r
id <- rep(seq(1, 100, 1), 2) ## individual ids for setting up a two period panel
t <- rep(seq(1, 2), 100) ## time periods
y <- rnorm(200) ## outcomes
dta <- data.frame(id = id, t = t, y = y) ## make into data frame
dta <- dta[-7, ] ## drop the 7th row from the dataset (which creates an unbalanced panel)
nrow(dta)
#> [1] 199
dta <- make_balanced_panel(dta, idname = "id", tname = "t")
nrow(dta) ## now all the observations with missing data in any period are dropped
#> [1] 198
```

## Example 3: Staggered treatment adoption

`get_group` identifies the time period in which each unit first becomes
treated. `check_staggered` verifies that treatment is absorbing (no
de-treatments).

``` r
n <- 100
id <- rep(seq_len(n), each = 4)
t <- rep(1:4, n)
## assign units to groups: 0 = never treated, 2 = treated from period 2, 3 = from period 3
g <- rep(sample(c(0, 2, 3), n, replace = TRUE), each = 4)
treat <- as.integer(t >= g & g > 0)
dta <- data.frame(id = id, t = t, treat = treat)

dta$group <- get_group(dta, idname = "id", tname = "t", treatname = "treat")
head(unique(dta[, c("id", "group")]))
#>    id group
#> 1   1     2
#> 5   2     3
#> 9   3     2
#> 13  4     0
#> 17  5     0
#> 21  6     2
check_staggered(dta, idname = "id", treatname = "treat")
#> [1] TRUE
```

## Example 4: Formula utilities

`rhs` extracts the right-hand side of a formula. `toformula` builds a
formula from variable name strings — useful when constructing formulas
programmatically.

``` r
ff <- y ~ x1 + x2 + x3
rhs(ff)
#> ~x1 + x2 + x3
#> <environment: 0x651fca30df70>
toformula("y", c("x1", "x2", "x3"))
#> y ~ x1 + x2 + x3
#> <environment: 0x651fca396268>
```

## Example 5: Working with lists

`get_list_element` extracts the same element from every entry in a list
— a common pattern when results are stored as a list of lists.

``` r
results <- lapply(1:5, function(i) list(est = i * 1.5, se = i * 0.1))
get_list_element(results, "est")
#> [[1]]
#> [1] 1.5
#> 
#> [[2]]
#> [1] 3
#> 
#> [[3]]
#> [1] 4.5
#> 
#> [[4]]
#> [1] 6
#> 
#> [[5]]
#> [1] 7.5
get_list_element(results, "se")
#> [[1]]
#> [1] 0.1
#> 
#> [[2]]
#> [1] 0.2
#> 
#> [[3]]
#> [1] 0.3
#> 
#> [[4]]
#> [1] 0.4
#> 
#> [[5]]
#> [1] 0.5
```

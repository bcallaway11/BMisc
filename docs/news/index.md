# Changelog

## BMisc (development version)

- Smaller footprint and improved performance.

  - Removed the `dplyr` and `tidyr` dependencies. The affected functions
    are now implemented with `data.table`, which was already a
    dependency, leading to a total reduction of ~20 packages
    (incl. strong recursive deps).

  - The workhorse panel “getter” functions
    ([`get_group()`](https://bcallaway11.github.io/BMisc/reference/get_group.md),
    [`get_Yit()`](https://bcallaway11.github.io/BMisc/reference/get_Yit.md),
    [`check_staggered()`](https://bcallaway11.github.io/BMisc/reference/check_staggered.md),
    etc.) have also been vectorized, yielding substantial performance
    improvements. On a simulated 200k row / 20k unit panel dataset, we
    observe 150-1400x speed gains, combined with an order of magnitude
    smaller memory allocation(s).

- Bug fixes:

  - [`check_staggered()`](https://bcallaway11.github.io/BMisc/reference/check_staggered.md)
    no longer (automatically) returns `FALSE` for panels with any
    treatment adoption. Previously, this function only returned `TRUE`
    when no unit ever changed treatment status.

  - [`get_Yit()`](https://bcallaway11.github.io/BMisc/reference/get_Yit.md)
    now returns `NA` for units that are not observed in period `tp`.
    Previously such units contributed nothing to the result, so the
    returned vector was shorter than the number of rows in the data.

- New features:

  - [`check_staggered()`](https://bcallaway11.github.io/BMisc/reference/check_staggered.md)
    and
    [`check_staggered_inner()`](https://bcallaway11.github.io/BMisc/reference/check_staggered_inner.md)
    gain an optional `tname` argument. The check compares consecutive
    rows, so it is only meaningful when a unit’s rows are in time order;
    supplying `tname` sorts them first. The default (`NULL`) keeps the
    previous assumption that the data is already ordered.

- Other changes:

  - [`get_principal_components()`](https://bcallaway11.github.io/BMisc/reference/get_principal_components.md)
    now returns units in sorted id order. This is only a change for data
    that is not already sorted by id, where it previously used order of
    first appearance, and it makes the function consistent with the
    other panel data getters.

  - [`get_first_difference()`](https://bcallaway11.github.io/BMisc/reference/get_first_difference.md)
    no longer adds a temporary `.lag` column to a copy of the input
    data, and now works when passed a `data.table`.

- Fixed silent row misalignment in several vectorized panel getters
  ([`get_group()`](https://bcallaway11.github.io/BMisc/reference/get_group.md),
  [`get_Yit()`](https://bcallaway11.github.io/BMisc/reference/get_Yit.md),
  [`get_principal_components()`](https://bcallaway11.github.io/BMisc/reference/get_principal_components.md),
  etc.) when units were not sorted by id; added
  [`sort_panel()`](https://bcallaway11.github.io/BMisc/reference/sort_panel.md)
  and
  [`set_balanced_panel()`](https://bcallaway11.github.io/BMisc/reference/set_balanced_panel.md),
  and
  [`make_balanced_panel()`](https://bcallaway11.github.io/BMisc/reference/make_balanced_panel.md)
  now preserves its input’s class instead of taking a
  `return_data.table` argument.

## BMisc 1.4.9

CRAN release: 2026-06-11

- Added [`.Deprecated()`](https://rdrr.io/r/base/Deprecated.html)
  wrappers to 13 legacy function names (e.g., `makeBalancedPanel`,
  `rhs.vars`). The old names remain fully functional but now emit
  deprecation warnings; removal is planned for a future version.

- Fixed bug in
  [`rhs()`](https://bcallaway11.github.io/BMisc/reference/rhs.md) where
  it was internally calling the deprecated
  [`rhs.vars()`](https://bcallaway11.github.io/BMisc/reference/rhs.vars.md)
  instead of
  [`rhs_vars()`](https://bcallaway11.github.io/BMisc/reference/rhs_vars.md),
  causing spurious deprecation warnings

- Fixed bug in
  [`panel2cs2()`](https://bcallaway11.github.io/BMisc/reference/panel2cs2.md)
  where outcomes could shift across unit boundaries in unbalanced panels
  when `balance_panel = FALSE`

- Updated license from GPL-2 to GPL-3

- Updated minimum R version from 3.1.0 to 4.1.0

- Moved `caret` from Imports to Suggests; it is only required for
  [`drop_collinear()`](https://bcallaway11.github.io/BMisc/reference/drop_collinear.md)
  and will prompt a clear error if not installed

- Added `@importFrom data.table .N .SD` in place of `@import data.table`
  to reduce namespace pollution

- Added roxygen2 examples to many previously undocumented exported
  functions

- Expanded test suite with coverage for panel utilities, distribution
  functions, weighted statistics, formula utilities, and Rcpp functions

## BMisc 1.4.8

CRAN release: 2025-02-04

- added function `weighted_combine_list` for computing weighed averages
  of a list of matrices or vectors

- modernized function names to snake case, keeping older function names
  available as internal functions

## BMisc 1.4.7

CRAN release: 2025-01-10

- moved functions `t2orig` and `orig2t` to `BMisc` from `pte` package

- add function `drop_collinear` to drop collinear columns from a matrix

- add function `get_principal_components` to get unit-specific principal
  components of time-varying variables

- add function `time_invariant_to_panel` for repeating time-invariant
  variables, mainly with the idea of adding them into a panel data set

- update get_group to allow for non-binary treatments

- add function `check_staggered` to check if a panel data set has
  staggered treatment adoption, this function can work with non-binary
  treatments

## BMisc 1.4.6

CRAN release: 2024-03-09

- Adds functions `get_group`, `get_YiGmin1`, and `get_Yi1` as utility
  functions for manipulating data. This is especially useful for
  settings with staggered treatment adoption.

- Add functions `get_lagYi` and `get_first_difference` as more utility
  functions for working with panel data.

- Add function `get_Yit` which recovers outcomes in period t for all
  units in a panel.

- Restore default of `makeBalancedPanel` to return a data.frame rather
  than a data.table. New function argument `return_data.table` allows
  for returning a data.table if this is the desired functionality from
  the user.

- Fixes documentation NOTE in `id2rownum`

## BMisc 1.4.5

CRAN release: 2022-07-11

- only execute examples for `subsample` and `blockBootSample` if `plm`
  package is available (thanks Achim Zeileis)

## BMisc 1.4.4

CRAN release: 2022-01-26

- faster version of `panel2cs2` (thanks Kyle Butts)

## BMisc 1.4.3

CRAN release: 2021-12-09

- added Rcpp multiplier_bootstrap function

- added TorF function, a vectorized version of isTRUE

- allow for additional arguments in combineDfs function

## BMisc 1.4.2

CRAN release: 2020-12-18

- changed package maintainer contact information

- added source_all function

## BMisc 1.4.1

CRAN release: 2020-04-01

- added getElementList function

## BMisc 1.4.0

- removed dependency on plm and formula.tools

- add function blockBootSample for block bootstrapping with panel data

- add option in makeDist to force the values of the distribution
  function be between 0 and 1

## BMisc 1.3.1

CRAN release: 2018-08-15

- Update rhs.vars to fix bug related to formulas like y~x+I(x^2)

- Update toformula to allow for no right hand side variables

## BMisc 1.3.0

CRAN release: 2018-07-16

- Added function to take distribution functions (ecdf objects) and turn
  them into step functions for the quantiles.

- Improved code for working with formulas

## BMisc 1.2.0

CRAN release: 2018-02-27

- Added function for obtaining a subsample of a panel data set

## BMisc 1.1.0

- Added function addCovToFormla which adds covariate(s) to a particular
  formula

## BMisc 1.0.1

CRAN release: 2017-06-14

- Removed dependency on qte package

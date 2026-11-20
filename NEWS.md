# BMisc 1.4.11

  * Bug fix: `get_group()` returned the period of a unit's first treated
    *row* rather than its earliest treated *period*, so it gave the wrong
    group when a unit's rows were not sorted by time. It now sorts by time
    first. Data sorted by unit and time were unaffected.

  * Documented the return values of `get_group()`, `get_YiGmin1()`,
    `get_Yi1()`, `get_Yibar()`, `get_Yibar_pre()`, `get_lagYi()` and
    `get_first_difference()`, and turned on Markdown in the documentation,
    so code in the help pages is no longer shown with literal backticks.

  * `multiplier_bootstrap()` is faster (#9, thanks to @kylebutts). It reads
    the influence function once, in blocks of rows, instead of once per
    bootstrap draw. With R's default (reference) BLAS it is about 1.3-1.5
    times faster; with an optimized BLAS (Accelerate, OpenBLAS, MKL) each
    block is a single matrix multiplication, which is several times faster
    again. The new `method` argument chooses between the two ("auto", the
    default, decides from the BLAS R uses). The Rademacher weights are now
    drawn differently, so for a given seed the bootstrap draws, and hence
    bootstrap standard errors and critical values in packages such as
    `did`, differ slightly from earlier versions.

  * `combine_ecdfs()` now computes the mixture exactly when `y.seq` is
    omitted (its new default): the mixture of step functions is a step
    function with a jump at every support point of every component, which
    are stacked, sorted and summed, at the cost of one sort. Quantiles of
    the result are then exact; on a grid `y.seq` they were rounded to the
    grid. Supplying `y.seq` gives the previous behavior.

  * `quantile()` now gives correct results for distribution functions made
    by `make_dist()`, `weighted_ecdf()` and `combine_ecdfs()` (#10). These
    objects have a new class, `BMisc_dist` (they are still `ecdf` objects),
    with a `quantile()` method that inverts the distribution function
    directly: the smallest point where it reaches the probability. Before,
    `quantile()` used `stats:::quantile.ecdf()`, which is only exact for
    `stats::ecdf()`; with ties or unequal weights it could be badly wrong
    (a 0/1 variable with P(1) = 0.6 had median 0). `type` is ignored, with a
    warning unless it is 1.

  * `invert_ecdf()` no longer returns the next-lower point between knots: it
    now returns the smallest point where the distribution function reaches
    the probability, matching `quantile(type = 1)`.

  * Faster `block_boot_sample()`, `orig2t()`, `t2orig()` and `weighted_ecdf()`,
    with the same results. On a 3,000-unit, 30-period panel,
    `block_boot_sample()` takes 0.03 s instead of 10 s.
    `weighted_ecdf()` agrees with the previous version to floating-point
    precision.

  * `orig2t()` and `t2orig()` now return `NA` for values that are not time
    periods. Previously they returned a list.

# BMisc 1.4.10

  * Smaller footprint and improved performance.

    - Removed the `dplyr` and `tidyr` dependencies. The affected functions are
      now implemented with `data.table`, which was already a dependency, leading
      to a total reduction of ~20 packages (incl. strong recursive deps).

    - The workhorse panel "getter" functions (`get_group()`, `get_Yit()`,
      `check_staggered()`, etc.) have also been vectorized, yielding
      substantial performance improvements. On a simulated 200k row / 20k unit
      panel dataset, we observe 150-1400x speed gains, combined with an order of
      magnitude smaller memory allocation(s).

  * Bug fixes:

    - `check_staggered()` no longer (automatically) returns `FALSE` for panels
      with any treatment adoption. Previously, this function only returned
      `TRUE` when no unit ever changed treatment status.

    - `get_Yit()` now returns `NA` for units that are not observed in period
      `tp`. Previously such units contributed nothing to the result, so the
      returned vector was shorter than the number of rows in the data.

  * New features:

    - `check_staggered()` and `check_staggered_inner()` gain an optional
      `tname` argument. The check compares consecutive rows, so it is only
      meaningful when a unit's rows are in time order; supplying `tname` sorts
      them first. The default (`NULL`) keeps the previous assumption that the
      data is already ordered.

  * Other changes:

    - `get_principal_components()` now returns units in sorted id order. This is
      only a change for data that is not already sorted by id, where it
      previously used order of first appearance, and it makes the function
      consistent with the other panel data getters.

    - `get_first_difference()` no longer adds a temporary `.lag` column to a
      copy of the input data, and now works when passed a `data.table`.

  * Fixed silent row misalignment in several vectorized panel getters
    (`get_group()`, `get_Yit()`, `get_principal_components()`, etc.) when
    units were not sorted by id; added `sort_panel()` and
    `set_balanced_panel()`, and `make_balanced_panel()` now preserves its
    input's class instead of taking a `return_data.table` argument.

# BMisc 1.4.9

  * Added `.Deprecated()` wrappers to 13 legacy function names (e.g.,
    `makeBalancedPanel`, `rhs.vars`). The old names remain fully functional
    but now emit deprecation warnings; removal is planned for a future version.

  * Fixed bug in `rhs()` where it was internally calling the deprecated
    `rhs.vars()` instead of `rhs_vars()`, causing spurious deprecation warnings

  * Fixed bug in `panel2cs2()` where outcomes could shift across unit boundaries
    in unbalanced panels when `balance_panel = FALSE`

  * Updated license from GPL-2 to GPL-3

  * Updated minimum R version from 3.1.0 to 4.1.0

  * Moved `caret` from Imports to Suggests; it is only required for
    `drop_collinear()` and will prompt a clear error if not installed

  * Added `@importFrom data.table .N .SD` in place of `@import data.table`
    to reduce namespace pollution

  * Added roxygen2 examples to many previously undocumented exported functions

  * Expanded test suite with coverage for panel utilities, distribution
    functions, weighted statistics, formula utilities, and Rcpp functions

# BMisc 1.4.8

  * added function `weighted_combine_list` for computing weighed averages of
    a list of matrices or vectors

  * modernized function names to snake case, keeping older function names available
    as internal functions

# BMisc 1.4.7

  * moved functions `t2orig` and `orig2t` to `BMisc` from `pte` package

  * add function `drop_collinear` to drop collinear columns from a matrix

  * add function `get_principal_components` to get unit-specific principal components of time-varying variables

  * add function `time_invariant_to_panel` for repeating time-invariant variables, mainly with the
    idea of adding them into a panel data set

  * update get_group to allow for non-binary treatments

  * add function `check_staggered` to check if a panel data set has staggered treatment adoption,
    this function can work with non-binary treatments

# BMisc 1.4.6

  * Adds functions `get_group`, `get_YiGmin1`, and `get_Yi1` as utility functions for manipulating data.  This is especially useful for settings with staggered treatment adoption.

  * Add functions `get_lagYi` and `get_first_difference` as more utility functions for working with panel data.

  * Add function `get_Yit` which recovers outcomes
    in period t for all units in a panel.

  * Restore default of `makeBalancedPanel` to return a data.frame rather than a data.table.  New function argument `return_data.table` allows for returning a data.table if this is the desired functionality from the user.

  * Fixes documentation NOTE in `id2rownum`

# BMisc 1.4.5

  * only execute examples for `subsample` and `blockBootSample` if `plm` package is available (thanks Achim Zeileis)

# BMisc 1.4.4

  * faster version of `panel2cs2` (thanks Kyle Butts)

# BMisc 1.4.3

  * added Rcpp multiplier_bootstrap function

  * added TorF function, a vectorized version of isTRUE

  * allow for additional arguments in combineDfs function

# BMisc 1.4.2

  * changed package maintainer contact information

  * added source_all function

# BMisc 1.4.1

  * added getElementList function

# BMisc 1.4.0

  * removed dependency on plm and formula.tools

  * add function blockBootSample for block bootstrapping with panel data

  * add option in makeDist to force the values of the distribution function be between 0 and 1

# BMisc 1.3.1

  * Update rhs.vars to fix bug related to formulas like y~x+I(x^2)

  * Update toformula to allow for no right hand side variables

# BMisc 1.3.0

 * Added function \code{invertEcdf} to take distribution functions (ecdf objects) and turn them into step functions for the quantiles.

 * Improved code for working with formulas

# BMisc 1.2.0

 * Added function \code{subsample} for obtaining a subsample of a panel data set

# BMisc 1.1.0

 * Added function addCovToFormla which adds covariate(s) to a particular formula

# BMisc 1.0.1

 * Removed dependency on qte package

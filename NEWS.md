# BMisc 1.4.6

  * Adds functions `get_group`, `get_YiGmin1`, and `get_Yi1` as utility functions for manipulating data.  This is especially useful for settings with staggered treatment adoption.

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

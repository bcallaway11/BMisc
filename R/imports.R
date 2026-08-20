#' @name BMisc
#' @title BMisc
#' @description A set of miscellaneous helper functions
#' @keywords internal
#' @useDynLib BMisc
#' @importFrom Rcpp sourceCpp
#' @import graphics
#' @import stats
#' @importFrom data.table .N .SD
"_PACKAGE"

utils::globalVariables(c(
  ".id", ".t", ".tn", ".y", ".g", ".tr", ".n", ".time",
  ".out", ".maxt", ".npre", ".pre", ".premean", ".allmean", ".nonzero", ".nz"
))

# =============================================================================
# Title: Multiplier bootstrap
# Description: multiplier_bootstrap() draws Rademacher-weighted bootstrap
#   replications of an influence function; the C++ work is in
#   multiplier_bootstrap_cpp() (src/BMisc.cpp).
# Author: Brant Callaway
# Last update: 2026-11-20
# Date created: 2026-09-09
# =============================================================================

#' @title multiplier_bootstrap
#'
#' @description A function that takes in an influence function (an
#' nxk matrix) and the number of bootstrap iterations and
#' returns a Bxk matrix of bootstrap results. This function
#' uses Rademacher weights.
#'
#' @details The influence function is read once, in blocks of rows, and each
#' block is combined with all \code{biters} draws at once. With an optimized
#' BLAS (Accelerate, OpenBLAS, MKL, BLIS) that is a single matrix
#' multiplication per block, which is much faster. With R's reference BLAS
#' it is not, and the draws are accumulated one at a time within each block
#' instead. \code{method = "auto"} picks between the two from
#' \code{extSoftVersion()[["BLAS"]]}. Both give the same draws for a given
#' seed (up to floating-point rounding), so the choice only affects speed.
#'
#' @param inf_func nxk matrix of (e.g., these could be a matrix
#'  containing the influence function for different parameter
#'  estimates)
#' @param biters the number of bootstrap iterations
#' @param method one of \code{"auto"} (the default), \code{"gemm"} to always
#'  use the blocked matrix multiplication, or \code{"loop"} to always
#'  accumulate one draw at a time.
#'
#' @return a Bxk matrix
#' @export
multiplier_bootstrap <- function(inf_func, biters,
                                 method = c("auto", "gemm", "loop")) {
  method <- match.arg(method)
  if (method == "auto") {
    # Accumulating a tile with `dgemm` is only a win when R uses an optimized
    # BLAS. Reference BLAS is scalar (and is what R ships on Windows), and
    # there its `dgemm` is no faster than one draw at a time, so fall back to
    # the loop. Anything not listed below also takes the loop.
    blas <- extSoftVersion()[["BLAS"]]
    optimized <- is.character(blas) && length(blas) == 1L && !is.na(blas) &&
      grepl("accelerate|veclib|openblas|mkl|blis", blas, ignore.case = TRUE)
    method <- if (optimized) "gemm" else "loop"
  }

  multiplier_bootstrap_cpp(inf_func, biters, method == "gemm")
}

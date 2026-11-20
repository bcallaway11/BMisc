#' @title multiplier_bootstrap
#'
#' @description A function that takes in an influence function (an
#' nxk matrix) and the number of bootstrap iterations and
#' returns a Bxk matrix of bootstrap results. This function
#' uses Rademechar weights.
#'
#' @details The influence function is read once and combined with all
#' `biters` draws at a time. That is only faster when R uses an optimized
#' BLAS (Accelerate, OpenBLAS, MKL, BLIS); the reference BLAS is scalar, and
#' there the draws are accumulated one at a time instead. `method = "auto"`
#' picks between the two from `extSoftVersion()[["BLAS"]]`; both give the
#' same draws, so the choice only affects speed.
#'
#' @param inf_func nxk matrix of (e.g., these could be a matrix
#'  containing the influence function for different parameter
#'  estimates)
#' @param biters the number of bootstrap iterations
#' @param method one of `"auto"` (the default), `"gemm"` to always use the
#'  tiled matrix multiplication, or `"loop"` to always accumulate one draw
#'  at a time.
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

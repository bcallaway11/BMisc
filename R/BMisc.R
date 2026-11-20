# =============================================================================
# Title: BMisc main functions
# Description: Panel-data helpers (balancing, block bootstrap, getters),
#   distribution helpers (weighted ECDFs, quantiles, combining ECDFs), time
#   recoding (orig2t/t2orig), and formula utilities.
# Author: Brant Callaway
# Last update: 2026-11-20
# Date created: 2017-02-07
# =============================================================================

#' @title Balance a Panel Data Set
#'
#' @description This function drops observations from data.frame
#'  that are not part of balanced panel data set. `data` is copied
#'  before dropping any rows, so the object passed in is left
#'  unmodified; see \code{\link{set_balanced_panel}} for an in-place
#'  alternative that avoids copying large inputs.
#'
#' @param data data.frame (or data.table) used in function
#' @param idname unique id
#' @param tname time period name
#' @examples
#' id <- rep(seq(1, 100), each = 2) # individual ids for setting up a two period panel
#' t <- rep(seq(1, 2), 100) # time periods
#' y <- rnorm(200) # outcomes
#' dta <- data.frame(id = id, t = t, y = y) # make into data frame
#' dta <- dta[-7, ] # drop the 7th row from the dataset (which creates an unbalanced panel)
#' dta <- make_balanced_panel(dta, idname = "id", tname = "t")
#'
#' @return a balanced panel, with the same class (`data.frame` or
#'  `data.table`) as `data`
#' @export
make_balanced_panel <- function(data, idname, tname) {
  if (!inherits(data, "data.frame")) {
    stop("data must be a data.frame")
  }
  was_data_table <- data.table::is.data.table(data)

  # as.data.table() always copies, even when `data` is already a
  # data.table, so the caller's object is never touched.
  dt <- data.table::as.data.table(data)
  nt <- length(unique(dt[[tname]]))
  out <- dt[, if (.N == nt) .SD, by = idname]

  # `out` is already a fresh, function-private object at this point, so
  # converting its class here is free (no further copy).
  if (!was_data_table) data.table::setDF(out)
  out
}

#' @title set_balanced_panel
#'
#' @description In-place version of \code{\link{make_balanced_panel}}
#'  for large panels where copying `data` is undesirable. Converts
#'  `data` to a data.table by reference (mutating the caller's object
#'  into a data.table as a side effect, if it is not one already)
#'  instead of copying it. The balanced result must still be captured
#'  from the return value, the same as any other function in this
#'  package (filtering out unbalanced units cannot itself happen
#'  without allocating the smaller, filtered result somewhere).
#'
#' @inheritParams make_balanced_panel
#'
#' @examples
#' id <- rep(seq(1, 100), each = 2)
#' t <- rep(seq(1, 2), 100)
#' y <- rnorm(200)
#' dta <- data.frame(id = id, t = t, y = y)
#' dta <- dta[-7, ]
#' dta <- set_balanced_panel(dta, idname = "id", tname = "t") # dta is now a data.table
#'
#' @return a balanced data.table
#' @export
set_balanced_panel <- function(data, idname, tname) {
  if (!inherits(data, "data.frame")) {
    stop("data must be a data.frame")
  }
  data.table::setDT(data)
  nt <- length(unique(data[[tname]]))
  data[, if (.N == nt) .SD, by = idname]
}

#' @title makeBalancedPanel
#'
#' @description Legacy version of `make_balanced_panel`,
#'  please use that function name going forward, though this will
#'  still work for now.
#'
#' @inheritParams make_balanced_panel
#'
#' @keywords internal
#' @export
makeBalancedPanel <- function(data, idname, tname) {
  .Deprecated("make_balanced_panel")
  make_balanced_panel(data = data, idname = idname, tname = tname)
}

#' @title sort_panel
#'
#' @description Sorts a panel data set by unit id and then time period.
#'  `data` is copied before sorting, so the object passed in is left
#'  unmodified. For large panels where copying is undesirable, sort in
#'  place instead with
#'  `data.table::setorderv(data, c(idname, tname))`.
#'
#' @inheritParams make_balanced_panel
#'
#' @examples
#' id <- rep(sample(1:5), each = 2) # units in a shuffled, unsorted order
#' t <- rep(c(2, 1), 5) # each unit's own periods out of time order
#' dta <- data.frame(id = id, t = t, y = rnorm(10))
#' dta <- sort_panel(dta, idname = "id", tname = "t")
#'
#' @return `data` sorted by `(idname, tname)`, with the same class
#'  (`data.frame` or `data.table`) as `data`
#' @export
sort_panel <- function(data, idname, tname) {
  if (!inherits(data, "data.frame")) {
    stop("data must be a data.frame")
  }
  was_data_table <- data.table::is.data.table(data)

  # as.data.table() always copies, even when `data` is already a
  # data.table, so the caller's object is never touched.
  out <- data.table::as.data.table(data)
  data.table::setorderv(out, c(idname, tname))

  # `out` is already a fresh, function-private object at this point, so
  # converting its class here is free (no further copy).
  if (!was_data_table) data.table::setDF(out)
  out
}

#' @title Panel Data to Repeated Cross Sections
#'
#' @description panel2cs takes a 2 period dataset and turns it
#'  into a cross sectional dataset.  The data includes the
#'  change in time varying variables between the
#'  time periods.  The default functionality
#'  is to keep all the variables from period 1
#'  and add all the variables listed by name in timevars
#'  from period 2 to those.
#'
#' @param data data.frame used in function
#' @param timevars vector of names of variables to keep
#' @param idname unique id
#' @param tname time period name
#'
#' @examples
#' id <- rep(seq(1, 50), 2)
#' t <- rep(seq(1, 2), each = 50)
#' y <- rnorm(100)
#' dta <- data.frame(id = id, t = t, y = y)
#' out <- panel2cs(dta, timevars = "y", idname = "id", tname = "t")
#' nrow(out)
#'
#' @return data.frame
#' @export
panel2cs <- function(data, timevars, idname, tname) {
  if (length(unique(data[, tname])) != 2) {
    stop("panel2cs only for 2 periods of panel data")
  }

  # balance the data, just in case
  data <- make_balanced_panel(data, idname, tname)

  # put everything in the right order,
  # so we can match it easily later on
  data <- data[order(data[, idname], data[, tname]), ]

  tdta <- aggregate(data[, timevars], by = list(data[, idname]), FUN = function(x) {
    x[2]
  })

  t1 <- unique(data[, tname])
  t1 <- t1[order(t1)][1]
  retdat <- subset(data, data[, tname] == t1)
  retdat$yt1 <- tdta[, 2]
  retdat$dy <- retdat$yt1 - retdat$y
  return(retdat)
}


#' @title Panel Data to Repeated Cross Sections
#'
#' @description panel2cs2 takes a 2 period dataset and turns it
#'  into a cross sectional dataset; i.e., long to wide.
#'  This function considers a particular case where there is some outcome
#'  whose value can change over time.  It returns the dataset from the first
#'  period with the outcome in the second period and the change in outcomes
#'  over time appended to it
#'
#' @param data data.frame used in function
#' @param yname name of outcome variable that can change over time
#' @param idname unique id
#' @param tname time period name
#' @param balance_panel whether to ensure that panel is balanced.  Default is TRUE, but code runs somewhat
#'  faster if this is set to be FALSE.
#'
#' @examples
#' id <- rep(seq(1, 50), 2)
#' t <- rep(seq(1, 2), each = 50)
#' y <- rnorm(100)
#' dta <- data.frame(id = id, t = t, y = y)
#' out <- panel2cs2(dta, yname = "y", idname = "id", tname = "t")
#' head(out[, c("id", ".y0", ".y1", ".dy")])
#'
#' @return data from first period with .y0 (outcome in first period),
#'  .y1 (outcome in second period), and .dy (change in outcomes
#'  over time) appended to it
#' @importFrom data.table :=
#' @export
panel2cs2 <- function(data, yname, idname, tname, balance_panel = TRUE) {
  # check that only 2 periods of data
  if (length(unique(data[[tname]])) != 2) {
    stop("panel2cs only for 2 periods of panel data")
  }

  # balance the data, just in case
  if (balance_panel) {
    data <- make_balanced_panel(data, idname, tname)
  }

  # data.table sorting (fast and memory efficient)
  data.table::setDT(data)
  data.table::setorderv(data, cols = c(idname, tname))

  # Fast global shift is valid for balanced two-period panels.  In unbalanced
  # panels, shift within id to avoid borrowing outcomes from the next unit.
  if (balance_panel) {
    data$.y1 <- data.table::shift(data[[yname]], -1)
  } else {
    data[, c(".y1") := data.table::shift(get(yname), -1), by = idname]
  }
  data$.y0 <- data[[yname]]
  data$.dy <- data$.y1 - data$.y0

  # Subset to first period after computing the logical index explicitly.
  first_period <- min(data[[tname]])
  first_period_idx <- data[[tname]] == first_period
  data <- data[first_period_idx, ]

  data
}



#' @title Convert Vector of ids into Vector of Row Numbers
#'
#' @description ids2rownum takes a vector of ids and converts it to the right
#'  row number in the dataset; ids should be unique in the dataset
#'  that is, don't pass the function panel data with multiple same ids
#'
#' @param ids vector of ids
#' @param data data frame
#' @param idname unique id
#'
#' @examples
#' ids <- seq(1, 1000, length.out = 100)
#' ids <- ids[order(runif(100))]
#' df <- data.frame(id = ids)
#' ids2rownum(df$id, df, "id")
#'
#' @return vector of row numbers
#' @export
ids2rownum <- function(ids, data, idname) {
  vapply(ids, id2rownum, 1.0, data = data, idname = idname)
}


#' @title Take particular id and convert to row number
#'
#' @description id2rownum takes an id and converts it to the right
#'  row number in the dataset; ids should be unique in the dataset
#'  that is, don't pass the function panel data with multiple same ids
#'
#' @param id a particular id
#' @param data data frame
#' @param idname unique id
#'
#' @keywords internal
#' @export
id2rownum <- function(id, data, idname) {
  which(data[, idname] == id)
}

#' @title Block Bootstrap
#'
#' @description make draws of all observations with the same id in a panel
#'  data context.  This is useful for bootstrapping with panel data.
#'
#' @param data data.frame from which you want to bootstrap
#' @param idname column in data which contains an individual identifier
#'
#' @return data.frame bootstrapped from the original dataset; this data.frame
#'  will contain new ids
#'
#' @examples
#' \dontshow{
#' if (!requireNamespace("plm")) {
#'   if (interactive() || is.na(Sys.getenv("_R_CHECK_PACKAGE_NAME_", NA))) {
#'     stop("package 'plm' is required for this example")
#'   } else {
#'     q()
#'   }
#' }
#' }
#' data("LaborSupply", package = "plm")
#' bbs <- block_boot_sample(LaborSupply, "id")
#' nrow(bbs)
#' head(bbs$id)
#'
#' @export
block_boot_sample <- function(data, idname) {
  uid <- unique(data[, idname])
  ids <- sample(uid, replace = TRUE)
  # Group row indices by id once and stack the sampled groups, instead of
  # scanning every row for each sampled id (O(n_ids x n_rows)). Rows, their
  # order, and the new ids are the same as stacking the per-id subsets.
  rows <- split(seq_len(nrow(data)), factor(data[, idname], levels = uid))
  pos <- match(ids, uid)
  out <- data[unlist(rows[pos], use.names = FALSE), , drop = FALSE]
  out[, idname] <- rep(seq_along(ids), lengths(rows)[pos])
  out
}

#' @title blockBootSample
#'
#' @description Legacy name for the function `block_boot_sample`,
#'  please use that function going forward.  This function will
#'  eventually be deleted
#'
#' @inheritParams block_boot_sample
#'
#' @keywords internal
#' @export
blockBootSample <- function(data, idname) {
  block_boot_sample(
    data = data,
    idname = idname
  )
}

#' @title Make a Distribution Function
#'
#' @description turn vectors of a values and their distribution function values
#'  into an ecdf.  Vectors should be the same length and both increasing.
#'
#' @param x vector of values
#' @param Fx vector of the distribution function values
#' @param sorted boolean indicating whether or not x is already sorted;
#'  computation is somewhat faster if already sorted
#' @param rearrange boolean indicating whether or not should monotize
#'  distribution function
#' @param force01 boolean indicating whether or not to force the values of
#'  the distribution function (i.e. Fx) to be between 0 and 1
#' @param method which method to pass to \code{approxfun} to approximate the
#'  distribution function.  Default is "constant"; other possible choice is
#'  "linear".  "constant" returns a step function, just like an empirical
#'  cdf; "linear" linearly interpolates between neighboring points.
#'
#' @examples
#' y <- rnorm(100)
#' y <- y[order(y)]
#' u <- runif(100)
#' u <- u[order(u)]
#' F <- make_dist(y, u)
#'
#' @return ecdf
#' @export
make_dist <- function(
    x,
    Fx,
    sorted = FALSE,
    rearrange = FALSE,
    force01 = FALSE,
    method = "constant") {
  if (!sorted) {
    tmat <- cbind(x, Fx)
    tmat <- tmat[order(x), , drop = FALSE]
    x <- tmat[, 1]
    Fx <- tmat[, 2]
  }

  if (force01) {
    Fx <- sapply(Fx, function(Fxval) max(min(Fxval, 1), 0))
  }

  if (rearrange) {
    Fx <- sort(Fx)
  }

  retF <- approxfun(x, Fx,
    method = method,
    yleft = 0, yright = 1, f = 0, ties = "ordered"
  )
  # "BMisc_dist" first, so quantile() uses quantile.BMisc_dist(), which
  # inverts the distribution function directly. stats:::quantile.ecdf()
  # rebuilds a sample by repeating each point round(nobs * mass) times,
  # which is only exact for stats::ecdf() (nobs = sample size); here nobs
  # is the number of points, so with ties or unequal weights it was wrong
  # (e.g. a 0/1 variable with P(1) = 0.6 had median 0).
  class(retF) <- c("BMisc_dist", "ecdf", "stepfun", class(retF))
  assign("nobs", length(x), envir = environment(retF))
  retF
}

#' @title Quantiles of a Distribution Function
#'
#' @description Quantiles of a distribution function created by
#'   \code{\link{make_dist}} (and so by \code{\link{weighted_ecdf}} and
#'   \code{\link{combine_ecdfs}}), computed by inverting it directly: for a
#'   step function, the smallest support point \eqn{y} with
#'   \eqn{F(y) \ge p} (the generalized inverse, \code{type = 1} in
#'   \code{\link[stats]{quantile}}); for \code{method = "linear"}, the
#'   inverse of the piecewise-linear \eqn{F}.
#'
#'   Sample-quantile types other than 1 interpolate between order statistics
#'   of a sample, which a weighted distribution function doesn't have, so
#'   \code{type} is ignored (with a warning if it isn't 1).
#'
#' @param x a distribution function from \code{\link{make_dist}}.
#' @param probs numeric vector of probabilities in \eqn{[0, 1]}.
#' @param names logical; if \code{TRUE}, name the result by \code{probs},
#'   as \code{\link[stats]{quantile}} does.
#' @param ... ignored, except \code{type} (see Description).
#'
#' @examples
#' F <- weighted_ecdf(c(0, 1), weights = c(0.4, 0.6))
#' quantile(F, 0.5) # 1
#'
#' @return numeric vector of quantiles.
#' @export
quantile.BMisc_dist <- function(x, probs = seq(0, 1, 0.25), names = TRUE,
                                ...) {
  type <- list(...)$type
  if (!is.null(type) && !identical(as.numeric(type), 1)) {
    warning("quantile() of a BMisc distribution function inverts it ",
            "directly (type = 1); type = ", type, " is ignored.",
            call. = FALSE)
  }
  if (any(probs < 0 | probs > 1, na.rm = TRUE)) {
    stop("'probs' outside [0,1]")
  }
  knots_x <- sort(unique(environment(x)$x))
  # running maximum: sums of weighted CDFs (combine_ecdfs()) can dip by
  # floating-point error, e.g. 1 then 1 - 1e-16
  cdf <- cummax(x(knots_x))
  # tolerance so a probability equal to a cumulative sum up to rounding
  # (e.g. 0.1 + 0.2 vs 0.3) lands on that point, not the next one
  idx <- findInterval(probs - 1e-10, cdf) + 1L
  idx <- pmin(idx, length(knots_x)) # F short of 1 by rounding: last point
  out <- knots_x[idx]
  # approxfun() stores its method as an integer code: 1 linear, 2 constant
  if (identical(environment(x)$method, 1L)) {
    # interpolate within the segment where F crosses p
    prev <- pmax(idx - 1L, 1L)
    seg <- idx > 1L & cdf[idx] > cdf[prev]
    out[seg] <- knots_x[prev[seg]] + (probs[seg] - cdf[prev[seg]]) /
      (cdf[idx[seg]] - cdf[prev[seg]]) * (knots_x[idx[seg]] - knots_x[prev[seg]])
  }
  out[is.na(probs)] <- NA
  if (names) {
    names(out) <- paste0(formatC(100 * probs, format = "fg", width = 1,
                                 digits = 7), "%")
  }
  out
}

#' @title makeDist
#'
#' @description Legacy name of `make_dist` function, please
#'  use that function instead.  This function will eventually
#'  be deleted.
#'
#' @inheritParams make_dist
#'
#' @keywords internal
#' @export
makeDist <- function(
    x,
    Fx,
    sorted = FALSE,
    rearrange = FALSE,
    force01 = FALSE,
    method = "constant") {
  .Deprecated("make_dist")
  make_dist(
    x = x,
    Fx = Fx,
    sorted = sorted,
    rearrange = rearrange,
    force01 = force01,
    method = method
  )
}


#' @title Invert Ecdf
#'
#' @description take an ecdf object and invert it to get a step-quantile
#'  function
#'
#' @param df an ecdf object
#'
#' @examples
#' y <- rnorm(100)
#' F <- ecdf(y)
#' Finv <- invert_ecdf(F)
#' Finv(0.5) ## approximate median
#'
#' @return stepfun object that contains the quantiles of the df
#'
#' @export
invert_ecdf <- function(df) {
  q <- knots(df)
  tau <- df(q)
  # right = TRUE with values (q_1, ..., q_n, q_n): p <= tau_1 gives q_1 and
  # p in (tau_k, tau_{k+1}] gives q_{k+1}, the smallest point with F >= p.
  # This used to be stepfun(tau, c(q_1, q)), which returned q_k on
  # [tau_k, tau_{k+1}), one step too low between knots (e.g. median 0 for a
  # 0/1 variable with P(1) = 0.6).
  stepfun(tau, c(q, q[length(q)]), right = TRUE)
}

#' @title invertEcdf
#'
#' @description Legacy function for `invert_ecdf`, please use that
#'  function instead.  This function will eventually be deleted.
#'
#' @inheritParams invert_ecdf
#'
#' @keywords internal
#' @export
invertEcdf <- function(df) {
  invert_ecdf(df)
}




#' @title Check Function
#'
#' @description The check function used for optimizing to get quantiles
#'
#' @param a vector to compute quantiles for
#' @param tau between 0 and 1, ex. .5 implies get the median
#'
#' @examples
#' x <- rnorm(100)
#' x[which.min(checkfun(x, 0.5))] ## should be around 0
#'
#' @return numeric value
#' @export
checkfun <- function(a, tau) {
  return(a * (tau - (1 * (a <= 0))))
}

#' @title Weighted Check Function
#'
#' @description Weights the check function
#'
#' @param q the value to check
#' @param cvec vector of data to compute quantiles for
#' @param tau between 0 and 1, ex. .5 implies get the median
#' @param weights the weights, weighted.checkfun normalizes the weights
#'  to sum to 1.
#'
#' @examples
#' x <- rnorm(100)
#' weighted_checkfun(0, x, tau = 0.5, weights = rep(1, 100))
#'
#' @return numeric
#' @export
weighted_checkfun <- function(q, cvec, tau, weights) {
  w <- weights
  retval <- mean(w * checkfun(cvec - q, tau))
  return(retval)
}

#' @title weighted.checkfun
#'
#' @description Legacy version of `weighted_checkfun`, please use that
#'  function instead.  This function will eventually be deleted.
#'
#' @inheritParams weighted_checkfun
#'
#' @keywords internal
#' @export
weighted.checkfun <- function(q, cvec, tau, weights) {
  .Deprecated("weighted_checkfun")
  weighted_checkfun(
    q = q,
    cvec = cvec,
    tau = tau,
    weights = weights
  )
}


#' @title Quantile of a Weighted Check Function
#'
#' @description Finds the quantile by optimizing the weighted check function
#'
#' @param tau between 0 and 1, ex. .5 implies get the median
#' @param cvec a vector to compute quantiles for
#' @param weights the weights, weighted.checkfun normalizes the weights
#'  to sum to 1.
#' @param norm normalize the weights so that they have mean of 1, default is
#'  to normalize
#'
#' @keywords internal
#' @export
weighted_quantile_inner <- function(tau, cvec, weights = NULL, norm = TRUE) {
  if (is.null(weights)) {
    weights <- 1
  }
  mw <- mean(weights)
  if (norm) {
    weights <- weights / mw
  }
  return(optimize(weighted_checkfun,
    lower = min(cvec), # nolint: indentation_linter. args to optimize() nested inside return()
    upper = max(cvec),
    cvec = cvec, tau = tau, weights = weights
  )$minimum)
}

#' @title weighted_quantile
#'
#' @description function to recover quantiles of a vector with weights
#'
#' @param tau a vector of values between 0 and 1
#' @param cvec a vector to compute quantiles for
#' @param weights the weights, weighted.checkfun normalizes the weights
#'  to sum to 1.
#' @param norm normalize the weights so that they have mean of 1, default is
#'  to normalize
#'
#' @examples
#' y <- rnorm(100)
#' w <- runif(100)
#' weighted_quantile(c(0.25, 0.5, 0.75), y, weights = w)
#'
#' @return vector of quantiles
#' @export
weighted_quantile <- function(tau, cvec, weights = NULL, norm = TRUE) {
  vapply(tau, weighted_quantile_inner, 1.0, cvec = cvec, weights = weights, norm = norm)
}

#' @title getWeightedQuantiles
#'
#' @description Legacy version of `weighted_quantile`, please use that
#'  function instead.  This function will eventually be deleted.
#'
#' @inheritParams weighted_quantile
#'
#' @keywords internal
#' @export
getWeightedQuantiles <- function(tau, cvec, weights = NULL, norm = TRUE) {
  .Deprecated("weighted_quantile")
  weighted_quantile(
    tau = tau,
    cvec = cvec,
    weights = weights,
    norm = norm
  )
}

#' @title Weighted Mean
#'
#' @description Get the mean applying some weights
#'
#' @param y a vector to compute the mean for
#' @param weights the vector of weights, can be NULL, then will just return mean
#' @param norm normalize the weights so that they have mean of 1, default is
#'  to normalize
#'
#' @examples
#' y <- rnorm(100)
#' w <- runif(100)
#' weighted_mean(y, weights = w)
#'
#' @return the weighted mean
#' @export
weighted_mean <- function(y, weights = NULL, norm = TRUE) {
  if (is.null(weights)) {
    weights <- 1
  }
  mw <- mean(weights)
  if (norm) {
    weights <- weights / mw
  }
  mean(weights * y)
}

#' @title getWeightedMean
#'
#' @description Legacy version of `weighted_mean`, please use
#'  that function instead.  This function will eventually be deleted.
#'
#' @inheritParams weighted_mean
#'
#' @keywords internal
#' @export
getWeightedMean <- function(y, weights = NULL, norm = TRUE) {
  .Deprecated("weighted_mean")
  weighted_mean(
    y = y,
    weights = weights,
    norm = norm
  )
}

#' @title Weighted Distribution Function
#'
#' @description Get a distribution function from a vector of values
#'  after applying some weights
#'
#' @param y a vector to compute the mean for
#' @param y.seq an optional vector of values to compute the distribution function
#'  for; the default is to use all unique values of y
#' @param weights the vector of weights, can be NULL, then will just return mean
#' @param norm normalize the weights so that they have mean of 1, default is
#'  to normalize
#'
#' @examples
#' y <- rnorm(100)
#' w <- runif(100)
#' F <- weighted_ecdf(y, weights = w)
#' F(0) ## approx 0.5
#'
#' @return ecdf
#' @export
weighted_ecdf <- function(y, y.seq = NULL, weights = NULL, norm = TRUE) {
  if (is.null(weights)) {
    weights <- 1
  }
  mw <- mean(weights)
  if (norm) {
    weights <- weights / mw
  }
  if (is.null(y.seq)) {
    y.seq <- unique(y)
    y.seq <- y.seq[order(y.seq)]
  }
  weights <- rep_len(weights, length(y))
  if (anyNA(y) || anyNA(weights) || anyNA(y.seq)) {
    # missing values propagate as before; findInterval() needs non-NA input
    dvals <- vapply(y.seq, FUN = function(x) {
      mean(weights * (y <= x))
    }, 1.0)
  } else {
    # F(x) = mean(w * 1{y <= x}): sort once and read cumulative weights at
    # each x, instead of a pass over y for every x (O(n log n), not O(n^2)).
    o <- order(y)
    cw <- c(0, cumsum(weights[o]))
    dvals <- cw[findInterval(y.seq, y[o]) + 1L] / length(y)
  }
  make_dist(y.seq, dvals)
}

#' @title getWeightedDf
#'
#' @description Legacy version of `weighted_ecdf`, please use that function
#'  instead.  This function will eventually be deleted.
#'
#' @inheritParams weighted_ecdf
#'
#' @keywords internal
#' @export
getWeightedDf <- function(y, y.seq = NULL, weights = NULL, norm = TRUE) {
  .Deprecated("weighted_ecdf")
  weighted_ecdf(
    y = y,
    y.seq = y.seq,
    weights = weights,
    norm = norm
  )
}

#' @title Cross Section to Panel
#'
#' @description Turn repeated cross sections data into panel data by
#'  imposing rank invariance; does not require
#'  that the inputs have the same length
#'
#' @param cs1 data frame, the first cross section
#' @param cs2 data frame, the second cross section
#' @param yname the name of the variable to calculate difference for (should be the same in each dataset)
#'
#' @examples
#' cs1 <- data.frame(y = rnorm(100))
#' cs2 <- data.frame(y = rnorm(100, mean = 1))
#' dy <- cs2panel(cs1, cs2, "y")
#' mean(dy) ## approx 1
#'
#' @return the change in outcomes over time
#' @export
cs2panel <- function(cs1, cs2, yname) {
  nu <- min(nrow(cs2), nrow(cs2))
  if (nu == nrow(cs2)) {
    ut <- cs2[, yname]
    ut <- ut[order(-ut)] ## orders largest to smallest
    ps <- seq(1, 0, length.out = length(ut)) ## orders largest to smallest
    utmin1 <- quantile(cs1[, yname], probs = ps, type = 1)
  } else {
    utmin1 <- cs2[, yname]
    utmin1 <- utmin1[order(-utmin1)] ## orders largest to smallest
    ps <- seq(1, 0, length.out = length(utmin1)) ## orders largest to smallest
    ut <- quantile(cs1[, yname], probs = ps, type = 1)
  }
  return(ut - utmin1)
}




#' @title Compare Variables across Groups
#'
#' @description \code{compare_binary} takes in a variable e.g. union
#' and runs bivariate regression of x on treatment (for summary statistics)
#'
#' @param x variables to run regression on
#' @param on binary variable
#' @param dta the data to use
#' @param w weights
#' @param report which type of report to make; diff is the difference between
#'  the two variables by group
#'
#'
#' @examples
#' dta <- data.frame(x = rnorm(100), treat = rep(c(0, 1), 50))
#' compare_binary("x", "treat", dta, report = "diff")
#'
#' @return matrix of results
#' @export
compare_binary <- function(x, on, dta, w = rep(1, nrow(dta)), report = c("diff", "levels", "both")) {
  if (inherits(dta[, x], "factor")) {
    df <- model.matrix(as.formula(paste0("~", x, "-1")), dta)
    vnames <- colnames(df)
    df <- data.frame(cbind(df, dta[, on]))
    colnames(df) <- c(vnames, "treat")
    t(simplify2array(lapply(vnames, compare_binary_inner, on = "treat", dta = df, w = w, report = report)))
  } else {
    compare_binary_inner(x, on, dta, w, report)
  }
}

#' @title compareBinary
#'
#' @description Legacy version of `compare_binary`, please use that
#'  function instead.  This function will eventually be deleted.
#'
#' @inheritParams compare_binary
#'
#' @keywords internal
#' @export
compareBinary <- function(x, on, dta, w = rep(1, nrow(dta)), report = c("diff", "levels", "both")) {
  .Deprecated("compare_binary")
  compare_binary(
    x = x,
    on = on,
    dta = dta,
    w = w,
    report = report
  )
}

#' @title Compare a single variable across two groups
#'
#' @description \code{compare_binary_inner} takes in a variable e.g. union
#' and runs bivariate regression of x on treatment (for summary statistics)
#'
#' @inheritParams compare_binary
#'
#' @return matrix of results
#'
#' @keywords internal
#' @export
compare_binary_inner <- function(x, on, dta, w = rep(1, nrow(dta)), report = c("diff", "levels", "both")) {
  coefmat <- summary(lm(as.formula(paste(x, on, sep = " ~ ")),
    data = dta,
    weights = w
  ))$coefficients
  if (report == "diff") {
    return(c(coefmat[1, 1] + coefmat[2, 1], coefmat[1, 1], abs(coefmat[2, 3]) > 1.96))
  } else if (report == "levels") { ## report the levels
    return(c(coefmat[1, 1] + coefmat[2, 1], coefmat[1, 1], abs(coefmat[2, 3]) > 1.96))
  } else if (report == "both") {
    return(c(coefmat[1, 1] + coefmat[2, 1], coefmat[1, 1], coefmat[2, 1], round(coefmat[2, 4], 3)))
  }
}

#-----------------------------------------------------------------------------
# functions for working with formulas
#-----------------------------------------------------------------------------


#' @title Right-hand Side Variables
#'
#' @description Take a formula and return a vector of the variables
#'  on the right hand side
#'
#' @param formula a formula
#'
#' @examples
#' ff <- yvar ~ x1 + x2
#' rhs_vars(ff)
#'
#' ff <- y ~ x1 + I(x1^2)
#' rhs_vars(ff)
#'
#' @return vector of variable names
#' @export
rhs_vars <- function(formula) {
  labels(terms(formula))
}

#' @title rhs.vars
#'
#' @description Legacy version of `rhs_vars`, please use that
#'  function instead.  This function will eventually be deleted.
#'
#' @param formla a formula
#'
#' @keywords internal
#' @export
rhs.vars <- function(formla) {
  .Deprecated("rhs_vars")
  rhs_vars(formla)
}

#' @title Left-hand Side Variables
#'
#' @description Take a formula and return a vector of the variables
#'  on the left hand side, it will return NULL for a one sided formula
#'
#' @inheritParams rhs_vars
#'
#' @examples
#' ff <- yvar ~ x1 + x2
#' lhs_vars(ff)
#' @return vector of variable names
#' @export
lhs_vars <- function(formula) {
  if (length(formula) == 2) {
    return(NULL) ## there is no lhs variable
  }
  all.vars(formula)[1]
}

#' @title lhs.vars
#'
#' @description Legacy version of `lhs_vars`, please use that function
#'  instead.  This function will eventually be deleted.
#'
#' @inheritParams lhs_vars
#'
#' @keywords internal
#' @export
lhs.vars <- function(formla) {
  .Deprecated("lhs_vars")
  lhs_vars(formla)
}

#' @title Right-hand Side of Formula
#'
#' @description Take a formula and return the right hand side
#'  of the formula
#'
#' @inheritParams rhs_vars
#'
#' @examples
#' ff <- yvar ~ x1 + x2
#' rhs(ff)
#'
#' @return a one sided formula
#' @export
rhs <- function(formula) {
  toformula(NULL, rhs_vars(formula))
}

#' @title Variable Names to Formula
#'
#' @description take a name for a y variable and a vector of names
#'  for x variables and turn them into a formula
#'
#' @param yname the name of the y variable
#' @param xnames vector of names for x variables
#'
#' @examples
#' toformula("yvar", c("x1", "x2"))
#'
#' ## should return yvar ~ 1
#' toformula("yvar", rhs_vars(~1))
#'
#' @return a formula
#' @export
toformula <- function(yname, xnames) {
  if (length(xnames) == 0) {
    return(as.formula(paste0(yname, " ~ 1")))
  }
  out <- paste0(yname, "~")
  xpart <- paste0(xnames, collapse = "+")
  out <- paste0(out, xpart)
  out <- as.formula(out)
  out
}

#' @title Add a Covariate to a Formula
#' @description \code{add_cov_to_formula} adds some covariates to a formula;
#'   covs should be a list of variable names
#'
#'
#' @param covs should be a list of variable names
#' @param formula which formula to add covariates to
#' @return formula
#'
#' @examples
#' ff <- y ~ x
#' add_cov_to_formula(list("w", "z"), ff)
#'
#' ff <- ~x
#' add_cov_to_formula("z", ff)
#'
#' @export
add_cov_to_formula <- function(covs, formula) {
  vs <- rhs_vars(formula) ## vector of x variable names
  vs <- c(vs, covs)
  formula <- toformula(lhs_vars(formula), vs)
  return(formula)
}

#' @title addCovToFormla
#'
#' @description Legacy version of `add_cov_to_formula`, please
#'  use that function instead.  This function will eventually be
#'  deleted.
#'
#' @inheritParams add_cov_to_formula
#'
#' @keywords internal
#' @export
addCovToFormla <- function(covs, formla) {
  .Deprecated("add_cov_to_formula")
  add_cov_to_formula(
    covs = covs,
    formula = formla
  )
}

#' @title Drop a Covariate from a Formula
#' @description \code{drop_cov_from_formula} adds drops some covariates from a
#' formula; covs should be a list of variable names
#'
#' @param covs should be a list of variable names
#' @param formula the formula to drop covariates from
#' @return formula
#'
#' @examples
#' ff <- y ~ x + w + z
#' drop_cov_from_formula(list("w", "z"), ff)
#'
#' drop_cov_from_formula("z", ff)
#'
#' @export
drop_cov_from_formula <- function(covs, formula) {
  vs <- rhs_vars(formula)
  vs <- vs[!(vs %in% covs)]
  toformula(lhs_vars(formula), vs)
}

#' @title dropCovFromFormla
#'
#' @description Legacy version of `drop_cov_from_formula`, please use
#'  that function instead.  This function will eventually be deleted.
#'
#' @inheritParams drop_cov_from_formula
#'
#' @keywords internal
#' @export
dropCovFromFormla <- function(covs, formla) {
  .Deprecated("drop_cov_from_formula")
  drop_cov_from_formula(covs = covs, formula = formla)
}

#' @title Combine Distribution Functions
#'
#' @description Combines distribution functions into their mixture
#'  \eqn{F(y) = \sum_c w_c F_c(y)}, with weights given by `weights`.
#'
#'  By default (`y.seq = NULL`) the mixture is exact: when every
#'  distribution function is a step function (as from [stats::ecdf()],
#'  [weighted_ecdf()] or [make_dist()]), so is the mixture, with a jump at
#'  each support point \eqn{y_{cj}} of each \eqn{F_c} equal to
#'  \eqn{w_c} times the jump of \eqn{F_c} there. These are stacked and
#'  sorted, and their cumulative sums give the mixture at every support
#'  point. `quantile()` of the result is then exact as well.
#'
#'  With `y.seq`, the mixture is instead evaluated only at the points
#'  `y.seq` (as in BMisc 1.4.10 and earlier), and its quantiles are rounded
#'  to those points; this also works for distribution functions that are
#'  not step functions.
#'
#' @param y.seq `NULL` (the default) for the exact mixture, or a sequence of
#'  y values at which to evaluate it
#' @param ecdflist list of ecdf objects (distribution functions) to combine
#' @param weights a vector of weights to put on each distribution function,
#'  which should sum to 1; if weights are not provided then equal weight is
#'  given to each distribution function. Distribution functions with weight
#'  0 are skipped in the exact mixture.
#' @param ... additional arguments that can be past to BMisc::make_dist
#'
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100, 1, 1)
#' Fx <- ecdf(x)
#' Fy <- ecdf(y)
#' both <- combine_ecdfs(ecdflist = list(Fx, Fy))
#' quantile(both, 0.5) # exact median of the mixture
#' plot(Fx, col = "green")
#' plot(Fy, col = "blue", add = TRUE)
#' plot(both, add = TRUE)
#'
#' # evaluated on a grid only
#' both_grid <- combine_ecdfs(seq(-2, 3, 0.1), list(Fx, Fy))
#'
#' @return ecdf (a `BMisc_dist` object, see [make_dist()])
#' @export
combine_ecdfs <- function(y.seq = NULL, ecdflist, weights = NULL, ...) {
  # Handle deprecated 'dflist' argument name passed via ...
  extra_args <- list(...)
  if ("dflist" %in% names(extra_args)) {
    if (!missing(ecdflist)) {
      stop("combine_ecdfs: cannot specify both 'ecdflist' and deprecated 'dflist'")
    }
    warning("combine_ecdfs: argument 'dflist' is deprecated; use 'ecdflist' instead",
            call. = FALSE)
    ecdflist <- extra_args[["dflist"]]
    extra_args[["dflist"]] <- NULL
  }
  # Guard against non-ecdf entries (e.g. NULL from uncomputable attgt cells),
  # which would crash silently inside the lapply below.
  not_ecdf <- !vapply(ecdflist, inherits, logical(1L), what = "ecdf")
  if (any(not_ecdf)) {
    bad_idx <- which(not_ecdf)
    stop(sprintf(
      "combine_ecdfs: element(s) %s of ecdflist are not ecdf objects (got: %s)",
      paste(bad_idx, collapse = ", "),
      paste(vapply(ecdflist[bad_idx], function(x) class(x)[1L], character(1L)),
            collapse = ", ")
    ))
  }
  if (is.null(weights)) {
    weights <- rep(1 / length(ecdflist), length(ecdflist))
  }
  if (length(weights) != length(ecdflist)) {
    stop("combine_ecdfs: weights must have one value per element of ecdflist")
  }
  if (is.null(y.seq)) {
    return(do.call(mix_ecdfs_exact, c(list(ecdflist, weights), extra_args)))
  }
  y.seq <- y.seq[order(y.seq)]
  df.valslist <- lapply(ecdflist, function(ddff) {
    ddff(y.seq)
  })
  df.valsmat <- simplify2array(df.valslist)
  for (i in seq_along(weights)) {
    df.valsmat[, i] <- df.valsmat[, i] * weights[i]
  }

  df.vals <- rowSums(df.valsmat)

  do.call(make_dist, c(list(y.seq, df.vals), extra_args))
}

# --- Internal: exact mixture of step-function distribution functions -------
# mix_ecdfs_exact(): the mixture sum_c w_c F_c of step functions, exactly.
#   Each F_c jumps at its support points y_cj (knots()) by
#   p_cj = F_c(y_cj) - F_c(y_c,j-1); the mixture jumps by w_c * p_cj there.
#   Stacking, sorting and taking cumulative sums gives the mixture at every
#   support point, at the cost of one sort, rather than evaluating every
#   F_c on a grid (which rounds quantiles to the grid) or at every pooled
#   point (quadratic). Arguments: ecdflist, weights (as in combine_ecdfs()),
#   and ... passed to make_dist(). Returns a make_dist() object.
mix_ecdfs_exact <- function(ecdflist, weights, ...) {
  use <- which(!is.na(weights) & weights != 0)
  if (length(use) == 0) {
    stop("combine_ecdfs: all weights are 0 or NA")
  }
  linear <- vapply(ecdflist[use], function(f) {
    identical(environment(f)$method, 1L)
  }, logical(1L))
  if (any(linear)) {
    stop("combine_ecdfs: the exact mixture needs step functions; for ",
         "make_dist(method = \"linear\") distribution functions, pass y.seq")
  }
  x <- lapply(ecdflist[use], stats::knots)
  p <- Map(function(f, xx, w) w * diff(c(0, f(xx))), ecdflist[use], x,
           weights[use])
  x <- unlist(x, use.names = FALSE)
  p <- unlist(p, use.names = FALSE)
  o <- order(x)
  x <- x[o]
  cdf <- cumsum(p[o])
  # one value per support point: the mixture at the last of tied points
  last <- !duplicated(x, fromLast = TRUE)
  make_dist(x[last], cdf[last], sorted = TRUE, ...)
}

#' @title combineDfs
#'
#' @description Legacy version of `combine_ecdfs`, please use that
#'  function instead.  This function will eventually be deleted.
#'
#' @inheritParams combine_ecdfs
#'
#' @keywords internal
#' @export
combineDfs <- function(y.seq, dflist, pstrat = NULL, ...) {
  .Deprecated("combine_ecdfs")
  combine_ecdfs(
    y.seq = y.seq,
    ecdflist = dflist,
    weights = pstrat, ...
  )
}

#' @title Subsample of Observations from Panel Data
#'
#' @description returns a subsample of a panel data set; in particular drops
#'  all observations that are not in \code{keepids}.  If it is not set,
#'  randomly keeps \code{nkeep} observations.
#'
#' @param dta a data.frame which is a balanced panel
#' @param idname the name of the id variable
#' @param tname the name of the time variable
#' @param keepids which ids to keep
#' @param nkeep how many ids to keep (only used if \code{keepids}
#'  is not set); the default is the number of unique ids
#'
#' @examples
#' \dontshow{
#' if (!requireNamespace("plm")) {
#'   if (interactive() || is.na(Sys.getenv("_R_CHECK_PACKAGE_NAME_", NA))) {
#'     stop("package 'plm' is required for this example")
#'   } else {
#'     q()
#'   }
#' }
#' }
#' data("LaborSupply", package = "plm")
#' nrow(LaborSupply)
#' unique(LaborSupply$year)
#' ss <- subsample(LaborSupply, "id", "year", nkeep = 100)
#' nrow(ss)
#'
#' @return a data.frame that contains a subsample of \code{dta}
#'
#' @export
subsample <- function(dta, idname, tname, keepids = NULL, nkeep = NULL) {
  ids <- unique(dta[, idname])

  if (is.null(keepids)) {
    if (is.null(nkeep)) nkeep <- length(ids)
    keepids <- sample(ids, size = nkeep)
  }

  retdta <- dta[dta[, idname] %in% keepids, ]
  retdta
}

#' @title Return Particular Element from Each Element in a List
#' @description a function to take a list and get a particular part
#'  out of each element in the list
#' @param listolists a list
#' @param whichone which item to get out of each list (can be numeric or name)
#'
#' @return list of all the elements 'whichone' from each list
#'
#' @examples
#' len <- 100 # number elements in list
#' lis <- lapply(1:len, function(l) list(x = (-l), y = l^2)) # create list
#' get_list_element(lis, "x")[1] # should be equal to -1
#' get_list_element(lis, 1)[1] # should be equal to -1
#'
#' @export
get_list_element <- function(listolists, whichone = 1) {
  lapply(listolists, function(l) l[[whichone]])
}

#' @title getListElement
#'
#' @description Legacy version of `get_list_element`, please use that function
#'  instead.  This function will eventually be deleted.
#'
#' @inheritParams get_list_element
#'
#' @keywords internal
#' @export
getListElement <- function(listolists, whichone = 1) {
  .Deprecated("get_list_element")
  get_list_element(listolists = listolists, whichone = whichone)
}

#' @title source_all
#'
#' @description Source all the files in a folder
#'
#' @param fldr path to a folder
#'
#' @export
source_all <- function(fldr) {
  sapply(paste0(fldr, list.files(fldr)), source)
}

#' @title TorF
#' @description A function to replace NA's with FALSE in vector of logicals
#' @param cond a vector of conditions to check
#' @param use_isTRUE whether or not to use a vectorized version
#'  of isTRUE.  This is generally slower but covers more cases.
#' @examples
#' TorF(c(TRUE, NA, FALSE)) ## NA becomes FALSE
#'
#' @return logical vector
#'
#' @export
TorF <- function(cond, use_isTRUE = FALSE) { # nolint: object_name_linter. mixed-case param is stable exported API
  if (!is.logical(cond)) stop("cond should be a logical vector")

  if (use_isTRUE) {
    cond <- sapply(cond, isTRUE)
  } else {
    cond[is.na(cond)] <- FALSE
  }
  cond
}


# The unit-level getters below all follow the same shape: aggregate `df` down
# to one row per unit (in sorted id order, which is what the previous
# dplyr::group_by() code produced), then `rep()` each unit's value back out
# over its rows.  They are vectorised equivalents of the exported `*_inner`
# functions, which remain available for operating on a single unit's data.

#' @title get_group_inner
#' @description Calculates the group for a particular unit
#' @param this_df a data.frame, for this function it should be specific to
#'  a particular unit
#' @inheritParams get_group
#' @keywords internal
#' @export
get_group_inner <- function(this_df, tname, treatname) {
  this_df <- as.data.frame(this_df)
  if (all(this_df[, treatname] == 0)) {
    return(0)
  }

  # the earliest treated period, whatever the row order
  as.numeric(min(this_df[this_df[, treatname] > 0, tname]))
}

#' @title get_group
#' @description A function to calculate a unit's group in a panel data setting
#'  with a binary treatment and staggered treatment adoption and where
#'  there is a column in the data indicating whether or not a unit is treated
#' @param df the data.frame used in the function
#' @param idname name of column that holds the unit id
#' @param tname name of column that holds the time period
#' @param treatname name of column with the treatment indicator
#' @examples
#' n <- 50
#' id <- rep(seq_len(n), each = 4)
#' t <- rep(1:4, n)
#' g <- rep(sample(c(0, 2, 3), n, replace = TRUE), each = 4)
#' treat <- as.integer(t >= g & g > 0)
#' dta <- data.frame(id = id, t = t, treat = treat)
#' dta$group <- get_group(dta, idname = "id", tname = "t", treatname = "treat")
#' head(unique(dta[, c("id", "group")]))
#' @return a numeric vector with one element per row of `df`: the unit's
#'  group, i.e. the first period in which it is treated, or 0 for units that
#'  are never treated
#' @export
get_group <- function(df, idname, tname, treatname) {
  dt <- data.table::data.table(
    .id = df[[idname]], .t = df[[tname]], .tr = df[[treatname]]
  )
  dt[, .nz := .tr != 0]
  units <- dt[, list(.n = .N, .nonzero = sum(.nz)), keyby = ".id"]
  # a unit's group is the first period in which it is observed as treated;
  # sort by time first, since rows need not be in time order within a unit
  first_treated <- unique(dt[.tr > 0][order(.id, .t)], by = ".id")
  out <- as.numeric(first_treated$.t[match(units$.id, first_treated$.id)])
  # units that are untreated in every period belong to group 0
  out[units$.nonzero == 0] <- 0
  # keyby sorts units by id, which can differ from df's row order; realign
  # by id rather than assuming df's rows are already grouped that way.
  out[match(dt$.id, units$.id)]
}

#' @title get_YiGmin1_inner
#' @description Calculates a units outcome (or also can be used for a covariate)
#'  in the period right before it becomes treated.  The unit's group must
#'  be specified at this point.  This function operates on a data.frame
#'  that is already local to a particular unit.
#' @param this_df a data.frame, for this function it should be specific to
#'  a particular unit
#' @inheritParams get_YiGmin1
#' @keywords internal
#' @export
get_YiGmin1_inner <- function(this_df, yname, tname, gname) {
  this_df <- as.data.frame(this_df)
  maxT <- max(this_df[, tname])
  this_group <- unique(this_df[, gname])
  YiGmin1 <- ifelse(this_group == 0,
    this_df[this_df[, tname] == maxT, yname],
    this_df[this_df[, tname] == (this_group - 1), yname]
  )
  YiGmin1
}

#' @title get_YiGmin1
#' @description A function to calculate outcomes for units in the period
#'  right before they become treated (this function can also be used to recover
#'  covariates, etc. in the period right before a unit becomes treated).
#'  For units that do not
#'  participate in the treatment (and therefore have group==0), they are
#'  assigned their outcome in the last period.
#' @param yname name of column containing the outcome (or other variable)
#'  for which to calculate its outcome in the immediate pre-treatment period
#' @param gname name of column containing the unit's group
#' @inheritParams get_group
#' @examples
#' n <- 50
#' id <- rep(seq_len(n), each = 4)
#' t <- rep(1:4, n)
#' y <- rnorm(n * 4)
#' g <- rep(sample(c(0, 2, 3), n, replace = TRUE), each = 4)
#' dta <- data.frame(id = id, t = t, y = y, group = g)
#' dta$YiGmin1 <- get_YiGmin1(dta, idname = "id", yname = "y",
#'                             tname = "t", gname = "group")
#' head(unique(dta[, c("id", "group", "YiGmin1")]))
#' @return a vector with one element per row of `df`: the unit's value of
#'  `yname` in period `group - 1` (in its last period if `group` is 0), or
#'  `NA` if the unit is not observed in that period
#' @export
get_YiGmin1 <- function(df, idname, yname, tname, gname) {
  dt <- data.table::data.table(
    .id = df[[idname]], .y = df[[yname]],
    .tn = as.numeric(df[[tname]]), .g = df[[gname]]
  )
  # NB the j expression is kept in a form data.table can optimise with GForce
  units <- dt[, list(.n = .N, .maxt = max(.tn), .g = .g[1L]), keyby = ".id"]
  # never-treated units (group 0) fall back to their last period
  units[, .tn := data.table::fifelse(.g == 0, .maxt, as.numeric(.g) - 1)]
  vals <- dt[units[, list(.id, .tn)], on = c(".id", ".tn"), mult = "first"]$.y
  # keyby sorts units by id, which can differ from df's row order; realign
  # by id rather than assuming df's rows are already grouped that way.
  vals[match(dt$.id, units$.id)]
}

#' @title get_Yi1_inner
#' @description Calculates a units outcome in the first time period.
#'  This function operates on a data.frame that is already local to a particular
#'  unit.
#' @inheritParams get_YiGmin1_inner
#' @keywords internal
#' @export
get_Yi1_inner <- function(this_df, yname, tname, gname) {
  this_df <- as.data.frame(this_df)
  minT <- min(this_df[, tname])
  Yi1 <- this_df[this_df[, tname] == minT, yname]
  Yi1
}

#' @title get_Yi1
#' @description A function to calculate outcomes for units in the first time
#'  period that is available in a panel data setting (this function can also
#'  be used to recover covariates, etc. in the first period).
#' @inheritParams get_YiGmin1
#' @examples
#' n <- 50
#' id <- rep(seq_len(n), each = 4)
#' t <- rep(1:4, n)
#' y <- rnorm(n * 4)
#' g <- rep(sample(c(0, 2, 3), n, replace = TRUE), each = 4)
#' dta <- data.frame(id = id, t = t, y = y, group = g)
#' dta$Yi1 <- get_Yi1(dta, idname = "id", yname = "y", tname = "t", gname = "group")
#' @return a vector with one element per row of `df`: the unit's value of
#'  `yname` in the first period in which it is observed
#' @export
get_Yi1 <- function(df, idname, yname, tname, gname) {
  dt <- data.table::data.table(.id = df[[idname]], .y = df[[yname]], .t = df[[tname]])
  units <- dt[, list(.n = .N), keyby = ".id"]
  # sorting by (id, time) puts each unit's earliest period in its first row
  firsts <- unique(dt[order(.id, .t)], by = ".id")
  vals <- firsts$.y[match(units$.id, firsts$.id)]
  # keyby sorts units by id, which can differ from df's row order; realign
  # by id rather than assuming df's rows are already grouped that way.
  vals[match(dt$.id, units$.id)]
}

#' @title get_Yit_inner
#' @description Calculates a units outcome in some particular period `tp`.
#'  This function operates on a data.frame that is already local to a particular
#'  unit.
#' @inheritParams get_YiGmin1_inner
#' @param tp The time period for which to get the outcome
#' @keywords internal
#' @export
get_Yit_inner <- function(this_df, tp, yname, tname) {
  this_df <- as.data.frame(this_df)
  Yit <- this_df[this_df[, tname] == tp, yname]
  Yit
}

#' @title get_Yit
#' @description A function to calculate outcomes for units in a particular
#'  time period `tp` in a panel data setting (this function can also
#'  be used to recover covariates, etc. in the first period).
#' @inheritParams get_YiGmin1
#' @inheritParams get_Yit_inner
#' @examples
#' n <- 50
#' id <- rep(seq_len(n), each = 4)
#' t <- rep(1:4, n)
#' y <- rnorm(n * 4)
#' dta <- data.frame(id = id, t = t, y = y)
#' Yit2 <- get_Yit(dta, tp = 2, idname = "id", yname = "y", tname = "t")
#' length(Yit2) ## n * 4
#'
#' @return a vector of outcomes in period t, the vector
#'  will have the length nT (i.e., this is returned for
#'  each element in the panel, not for a particular period);
#'  `NA` for units not observed in period `tp`
#' @export
get_Yit <- function(df, tp, idname, yname, tname) {
  dt <- data.table::data.table(.id = df[[idname]], .y = df[[yname]], .t = df[[tname]])
  units <- dt[, list(.n = .N), keyby = ".id"]
  at_tp <- unique(dt[.t == tp], by = ".id")
  # units that are not observed in period tp get NA
  vals <- at_tp$.y[match(units$.id, at_tp$.id)]
  # keyby sorts units by id, which can differ from df's row order; realign
  # by id rather than assuming df's rows are already grouped that way.
  vals[match(dt$.id, units$.id)]
}

#' @title get_Yibar_inner
#' @description Calculates a units average outcome across all periods.
#'  This function operates on a data.frame that is already local to a particular
#'  unit.
#' @inheritParams get_YiGmin1_inner
#' @keywords internal
#' @export
get_Yibar_inner <- function(this_df, yname) {
  this_df <- as.data.frame(this_df)
  mean(this_df[, yname])
}

#' @title get_Yibar
#' @description A function to calculate the average outcome across all time
#' periods separately for each unit in a panel data setting (this function can also
#'  be used to recover covariates, etc.).
#' @inheritParams get_YiGmin1
#' @examples
#' n <- 50
#' id <- rep(seq_len(n), each = 4)
#' t <- rep(1:4, n)
#' y <- rnorm(n * 4)
#' dta <- data.frame(id = id, t = t, y = y)
#' dta$Yibar <- get_Yibar(dta, idname = "id", yname = "y")
#' @return a numeric vector with one element per row of `df`: the unit's
#'  average of `yname` over all of its periods
#' @export
get_Yibar <- function(df, idname, yname) {
  dt <- data.table::data.table(.id = df[[idname]], .y = df[[yname]])
  units <- dt[, list(.n = .N, .out = mean(.y)), keyby = ".id"]
  # keyby sorts units by id, which can differ from df's row order; realign
  # by id rather than assuming df's rows are already grouped that way.
  units$.out[match(dt$.id, units$.id)]
}

#' @title get_Yibar_pre_inner
#' @description Calculates a unit's average outcome in pre-treatment periods
#'  (or also can be used for a covariate).  The unit's group must
#'  be specified at this point.  This function operates on a data.frame
#'  that is already local to a particular unit.
#' @param this_df a data.frame, for this function it should be specific to
#'  a particular unit
#' @inheritParams get_YiGmin1_inner
#' @keywords internal
#' @export
get_Yibar_pre_inner <- function(this_df, yname, tname, gname) {
  this_df <- as.data.frame(this_df)
  this_group <- unique(this_df[, gname])
  Yibarpre <- ifelse(this_group == 0,
    mean(this_df[, yname]),
    mean(this_df[this_df[, tname] < this_group, yname])
  )
  Yibarpre
}

#' @title get_Yibar_pre
#' @description A function to calculate average outcomes for units in
#'  their pre-treatment periods (this function can also be used to recover
#'  pre-treatment averages of covariates, etc.).
#'  For units that do not
#'  participate in the treatment (and therefore have group==0), the
#'  function calculates their overall average outcome.
#' @param yname name of column containing the outcome (or other variable)
#'  for which to calculate its outcome in the immediate pre-treatment period
#' @param gname name of column containing the unit's group
#' @inheritParams get_YiGmin1
#' @examples
#' n <- 50
#' id <- rep(seq_len(n), each = 4)
#' t <- rep(1:4, n)
#' y <- rnorm(n * 4)
#' g <- rep(sample(c(0, 2, 3), n, replace = TRUE), each = 4)
#' dta <- data.frame(id = id, t = t, y = y, group = g)
#' dta$Yibarpre <- get_Yibar_pre(dta, idname = "id", yname = "y",
#'                                tname = "t", gname = "group")
#' @return a numeric vector with one element per row of `df`: the unit's
#'  average of `yname` over periods before its group (over all periods for
#'  never-treated units, `NaN` for treated units with no pre-treatment
#'  periods)
#' @export
get_Yibar_pre <- function(df, idname, yname, tname, gname) {
  dt <- data.table::data.table(
    .id = df[[idname]], .y = df[[yname]], .t = df[[tname]], .g = df[[gname]]
  )
  dt[, .pre := .t < .g]
  units <- dt[, list(
    .n = .N, .g = .g[1L], .allmean = mean(.y), .npre = sum(.pre)
  ), keyby = ".id"]
  pre <- dt[(.pre), list(.premean = mean(.y)), keyby = ".id"]
  out <- pre$.premean[match(units$.id, pre$.id)]
  # treated units with no pre-treatment periods average over nothing
  out[units$.npre == 0] <- NaN
  # never-treated units (group 0) average over all of their periods
  never_treated <- units$.g == 0
  out[never_treated] <- units$.allmean[never_treated]
  # keyby sorts units by id, which can differ from df's row order; realign
  # by id rather than assuming df's rows are already grouped that way.
  out[match(dt$.id, units$.id)]
}

#' @title get_lagYi
#' @description A function that calculates lagged outcomes in a panel data setting.
#'  If the data.frame that is passed in has nxT rows, the resulting vector will
#'  also have nxT elements with one element for each unit set to be NA
#' @inheritParams get_Yi1
#' @param nlags The number of periods to lag.  The default is 1, which computes
#'  the lag from the previous period.
#' @examples
#' n <- 50
#' id <- rep(seq_len(n), each = 4)
#' t <- rep(1:4, n)
#' y <- rnorm(n * 4)
#' dta <- data.frame(id = id, t = t, y = y)
#' dta$lag_y <- get_lagYi(dta, idname = "id", yname = "y", tname = "t")
#' @return a vector with one element per row of `df`: the value of `yname`
#'  `nlags` periods earlier for the same unit (ordered by `tname`), `NA` for
#'  each unit's first `nlags` periods
#' @export
get_lagYi <- function(df, idname, yname, tname, nlags = 1) {
  y <- df[[yname]]
  # Sort once by (id, time), shift within unit, then scatter the result back
  # into the original row order.
  ord <- order(df[[idname]], df[[tname]])
  sorted <- data.table::data.table(.id = df[[idname]][ord], .y = y[ord])
  y[ord] <- sorted[, data.table::shift(.y, n = nlags), by = ".id"]$V1
  y
}

#' @title get_first_difference
#' @description A function that calculates the first difference in a panel data
#'  setting.  If the data.frame that is passed in has nxT rows, the resulting
#'  vector will also have nxT elements with one element for each unit set to be
#'  NA.
#' @inheritParams get_lagYi
#' @examples
#' n <- 50
#' id <- rep(seq_len(n), each = 4)
#' t <- rep(1:4, n)
#' y <- rnorm(n * 4)
#' dta <- data.frame(id = id, t = t, y = y)
#' dy <- get_first_difference(dta, idname = "id", yname = "y", tname = "t")
#' @return a vector with one element per row of `df`: the change in `yname`
#'  from the unit's previous period (ordered by `tname`), `NA` for each
#'  unit's first period
#' @export
get_first_difference <- function(df, idname, yname, tname) {
  df[[yname]] - get_lagYi(df, idname, yname, tname)
}

#' @title time_invariant_to_panel
#'
#' @description This function takes a time-invariant variable and repeats it
#'  for each period in a panel data set.
#'
#' @param x a vector of length equal to the number of unique ids in df.
#' @inheritParams get_lagYi
#' @param balanced_panel a logical indicating whether the panel is balanced.
#'  If TRUE, the function will optimize the repetition process.  Default
#'  is TRUE.
#'
#' @examples
#' n <- 50
#' id <- rep(seq_len(n), each = 4)
#' t <- rep(1:4, n)
#' dta <- data.frame(id = id, t = t)
#' x_unit <- rnorm(n)
#' x_panel <- time_invariant_to_panel(x_unit, dta, idname = "id")
#' length(x_panel) ## n * 4
#'
#' @return a vector of length equal to the number of rows in df.
#' @export
time_invariant_to_panel <- function(x, df, idname, balanced_panel = TRUE) {
  # Ensure that x has the same length as the number of unique ids
  unique_ids <- unique(df[[idname]])
  if (length(x) != length(unique_ids)) {
    stop("The length of x must be equal to the number of unique ids in df.")
  }

  # If the panel is balanced, optimize the repetition process
  if (balanced_panel) {
    n_per_id <- nrow(df) / length(unique_ids)
    out <- rep(x, each = n_per_id)
  } else {
    x_id_map <- cbind.data.frame(x, id = unique_ids)
    out <- merge(df, x_id_map, by = "id")[, "x"]
  }

  return(out)
}

#' @title check_staggered_inner
#'
#' @description A helper function to check whether treatment is absorbing for
#'  a single unit; that is, whether the unit never reverts from treated back
#'  to untreated.
#'
#' @inheritParams get_group_inner
#' @param tname name of column that holds the time period.  If supplied, the
#'  unit's rows are sorted by it before checking.  If `NULL` (the default),
#'  the rows are assumed to already be in time order.
#'
#' @keywords internal
#' @export
check_staggered_inner <- function(this_df, treatname, tname = NULL) {
  this_df <- as.data.frame(this_df)
  treat <- this_df[, treatname]
  if (!is.null(tname)) {
    treat <- treat[order(this_df[, tname])]
  }
  # treatment is staggered as long as it is absorbing, i.e. as long as it
  # never switches back off from one period to the next
  !any(diff(treat) < 0, na.rm = TRUE)
}

#' @title check_staggered
#'
#' @description A function to check whether treatment is staggered in a panel
#'  data set; that is, whether treatment is absorbing, so that no unit ever
#'  reverts from treated back to untreated.
#'
#' @inheritParams get_group
#' @param tname name of column that holds the time period.  If supplied, each
#'  unit's rows are sorted by it before checking.  If `NULL` (the default),
#'  the rows are assumed to already be in time order.
#'
#' @examples
#' n <- 50
#' id <- rep(seq_len(n), each = 4)
#' t <- rep(1:4, n)
#' g <- rep(sample(c(0, 2, 3), n, replace = TRUE), each = 4)
#' treat <- as.integer(t >= g & g > 0)
#' dta <- data.frame(id = id, t = t, treat = treat)
#' check_staggered(dta, idname = "id", treatname = "treat")
#'
#' ## treatment that switches back off is not staggered
#' dta_rev <- data.frame(
#'   id = c(1, 1, 2, 2), t = c(1, 2, 1, 2), treat = c(0, 1, 1, 0)
#' )
#' check_staggered(dta_rev, idname = "id", treatname = "treat")
#'
#' @return a logical indicating whether treatment is staggered
#' @export
check_staggered <- function(df, idname, treatname, tname = NULL) {
  id <- df[[idname]]
  treat <- df[[treatname]]
  if (length(treat) < 2L) {
    return(TRUE)
  }
  # bring each unit's rows together, in time order when tname is supplied.
  # order() is stable, so rows otherwise keep the order they came in.
  ord <- if (is.null(tname)) order(id) else order(id, df[[tname]])
  id <- id[ord]
  treat <- treat[ord]
  # mirrors check_staggered_inner(): within a unit, treatment must never fall
  # from one period to the next
  fell <- treat < data.table::shift(treat)
  first_of_unit <- c(TRUE, id[-1L] != id[-length(id)])
  !any(fell & !first_of_unit, na.rm = TRUE)
}

#' Matrix-Vector Multiplication
#'
#' This function multiplies a matrix by a vector and returns a numeric vector.
#'
#' @param A an nxk matrix.
#' @param v a vector (can be stored as numeric or as a kx1 matrix)
#'
#' @return A numeric vector resulting from the multiplication of the matrix by the vector.
#' @export
#'
#' @examples
#' A <- matrix(1:9, nrow = 3, ncol = 3)
#' v <- c(2, 4, 6)
#' mv_mult(A, v)
mv_mult <- function(A, v) {
  drop(A %*% v)
}

#' @title t2orig_inner
#'
#' @description A helper function to switch from "new" t values to
#' original t values for a single t.
#'
#' @param t a single time period to convert back to original time
#' @inheritParams t2orig
#'
#' @keywords internal
#' @export
t2orig_inner <- function(t, original_time.periods) {
  new_time.periods <- seq(1, length(unique(original_time.periods)))
  unique(c(original_time.periods, 0))[which(c(new_time.periods, 0) == t)]
}

#' @title orig2t_inner
#'
#' @description A helper function to switch from original t values to
#' "new" t values (which are just time periods going from 1 to total
#' number of available periods).
#'
#' @param orig a single original time period to convert to new time period
#' @inheritParams orig2t
#'
#' @keywords internal
#' @export
orig2t_inner <- function(orig, original_time.periods) {
  new_time.periods <- seq(1, length(unique(original_time.periods)))
  c(new_time.periods, 0)[which(unique(c(original_time.periods, 0)) == orig)]
}

#' @title t2orig
#'
#' @description A helper function to switch from "new" t values to
#' original t values.  This allows for periods not being exactly spaced
#' apart by 1.
#'
#' @param t a vector of time periods to convert back to original time
#'  periods.
#' @param original_time.periods vector containing all original time periods.
#'
#' @examples
#' original_time.periods <- c(2001, 2003, 2005, 2007)
#' t2orig(1:4, original_time.periods) ## returns c(2001, 2003, 2005, 2007)
#'
#' @return original time period converted from new time period; \code{NA}
#'  for values of \code{t} that do not correspond to a time period.
#'
#' @export
t2orig <- function(t, original_time.periods) {
  # check that orignal time periods are equally spaced
  if (length(unique(diff(original_time.periods))) > 1) {
    warning("original_time.periods are unequally spaced, some downstream functions may not work as expected.")
  }
  # vectorized t2orig_inner(): new period k maps to the k-th unique original
  # period, and 0 (never treated) maps to 0
  new_time.periods <- c(seq_along(unique(original_time.periods)), 0)
  unique(c(original_time.periods, 0))[match(t, new_time.periods)]
}

#' @title orig2t
#'
#' @description A helper function to switch from original time periods to
#'  "new" time periods (which are just time periods going from 1 to total
#'  number of available periods).  This allows for periods not being
#'  exactly spaced apart by 1.
#'
#' @inheritParams t2orig
#' @param orig a vector of original time periods to convert to new time periods.
#'
#' @examples
#' original_time.periods <- c(2001, 2003, 2005, 2007)
#' orig2t(c(2001, 2005), original_time.periods) ## returns c(1, 3)
#'
#' @return new time period converted from original time period; \code{NA}
#'  for values of \code{orig} that are not among \code{original_time.periods}
#'  (or 0).
#'
#' @export
orig2t <- function(orig, original_time.periods) {
  # check that orignal time periods are equally spaced
  if (length(unique(diff(original_time.periods))) > 1) {
    warning("original_time.periods are unequally spaced, some downstream functions may not work as expected.")
  }
  # vectorized orig2t_inner(): the k-th unique original period maps to k,
  # and 0 (never treated) maps to 0
  new_time.periods <- c(seq_along(unique(original_time.periods)), 0)
  new_time.periods[match(orig, unique(c(original_time.periods, 0)))]
}

#' @title drop_collinear
#' @description A function to check for multicollinearity and drop collinear terms
#'  from a matrix
#' @param matrix a matrix for which the function will remove collinear columns
#' @examples
#' \donttest{
#'   if (requireNamespace("caret", quietly = TRUE)) {
#'     X <- cbind(1:5, 2 * (1:5), rnorm(5))
#'     colnames(X) <- c("x1", "x2", "x3")
#'     drop_collinear(X) ## x2 dropped as collinear with x1
#'   }
#' }
#' @return a matrix with collinear columns removed
#' @export
drop_collinear <- function(matrix) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package 'caret' is required for drop_collinear(). Install it with install.packages('caret').")
  }
  # Find the columns that are collinear
  collinear_info <- caret::findLinearCombos(matrix)

  if (!is.null(collinear_info$remove)) {
    # Extract the names of the columns to be removed
    dropped_covariates <- colnames(matrix)[collinear_info$remove]

    # Print a warning message with the names of the dropped covariates
    warning("The following covariates were dropped due to collinearity: ", paste(dropped_covariates, collapse = ", "))

    # Drop the collinear columns
    matrix <- matrix[, -collinear_info$remove]
  }

  return(matrix)
}

#' @title get_principal_components
#' @description A function to calculate unit-specific principal components, given panel data
#' @param xformula a formula specifying the variables to use in the principal component analysis
#' @param data a data.frame containing the panel data
#' @param idname the name of the column containing the unit id
#' @param tname the name of the column containing the time period
#' @param n_components the number of principal components to retain, the default is NULL which
#'  will result in all principal components being retained
#' @param ret_wide whether to return the data in wide format (where the number of rows
#'  is equal to `n = length(unique(data[[idname]]))` or long format (where the number
#'  of rows is equal to `nT = nrow(data)`).  The default is FALSE, so that long data
#'  is returned by default.
#' @param ret_id whether to return the id column in the output data.frame.  The default is FALSE.
#' @examples
#' n <- 20
#' id <- rep(seq_len(n), each = 4)
#' t <- rep(1:4, n)
#' x1 <- rnorm(n * 4)
#' dta <- data.frame(id = id, t = t, x1 = x1)
#' pcs <- get_principal_components(~x1, dta, idname = "id", tname = "t")
#' dim(pcs)
#'
#' @return a data.frame containing the original data with the principal components appended
#' @export
get_principal_components <- function(
    xformula, data, idname, tname,
    n_components = NULL, ret_wide = FALSE, ret_id = FALSE) {
  X <- model.matrix(xformula, data)
  # drop intercept if it is included
  if (all(X[, 1] == 1)) {
    X <- X[, -1, drop = FALSE]
  }
  nperiods <- length(unique(data[[tname]]))
  # handle number of components to return
  if (is.null(n_components)) {
    n_components <- nperiods
  }
  pc_list <- list()
  for (i in seq_len(ncol(X))) {
    this_x_name <- colnames(X)[i]
    x <- X[, i]
    df <- data.table::data.table(.id = data[[idname]], .time = data[[tname]], x = x)
    wide_data <- data.table::dcast(df, .id ~ .time, value.var = "x")
    .id <- wide_data$.id # nolint: object_name_linter. dot-prefix col name from dcast
    pca_inner <- prcomp(
      as.matrix(wide_data[, !".id"]),
      center = FALSE, scale. = FALSE
    )
    princ_comp <- pca_inner$x[, 1:n_components]
    colnames(princ_comp) <- paste0(this_x_name, "_", colnames(princ_comp))
    pc_list[[i]] <- princ_comp
  }
  pc_data <- do.call(cbind.data.frame, pc_list)
  # dcast() sorts rows by .id, which can differ from the order units first
  # appear in `data`; keep .id attached so the long-format output below can
  # be realigned to each row's own unit instead of assumed positionally.
  pc_data <- cbind.data.frame(.id, pc_data)

  if (ret_wide) {
    if (!ret_id) pc_data$.id <- NULL
    return(pc_data)
  }

  out <- pc_data[match(data[[idname]], pc_data$.id), , drop = FALSE]
  rownames(out) <- NULL
  if (!ret_id) out$.id <- NULL
  out
}

#' @title weighted_combine_list
#'
#' @description A function that takes in either a list of vectors or matrices
#'  and computes a weighted average of them, where the weights are applied to
#'  every element in the list.
#'
#' @param l a list that contains either vectors or matrices of the same dimension
#'  that are to be combined
#'
#' @param w a vector of weights, the weights should have the same number
#'  of elements as `length(l)`
#'
#' @param normalize_weights whether or not to force the weights to sum to 1,
#'  default is true
#'
#' @examples
#' l <- list(c(1, 2, 3), c(4, 5, 6))
#' weighted_combine_list(l, w = c(0.5, 0.5)) ## returns c(2.5, 3.5, 4.5)
#'
#' @return matrix or vector corresponding to the weighted average of
#'  all of the elements in `l`
#'
#' @export
weighted_combine_list <- function(l, w, normalize_weights = TRUE) {
  # make sure the arguments passed in are compatible with the function
  if (!is.list(l)) stop("`l` should be a list")
  if (!is.numeric(w)) stop("`w` should be a numeric vector")
  if (!(is.matrix(l[[1]]) || is.numeric(l[[1]]))) stop("`l` should contain numeric vectors or matrices")
  if (is.numeric(l[[1]]) && is.vector(l[[1]])) {
    unique_l_length <- unique(sapply(l, length))
    if (!(length(unique_l_length) == 1)) stop("all elements of `l` should have the same length")
  }
  if (is.matrix(l[[1]])) {
    dim_mat <- sapply(l, dim)
    all_same_dim <- all(apply(dim_mat, 2, function(col) all(col == dim_mat[, 1])))
    if (!all_same_dim) stop("all elements of `l` should have the same dimension")
  }
  if (length(l) != length(w)) stop("`l` and `w` should have the same length")

  if (normalize_weights) {
    w <- w / sum(w)
  }

  weighted_list <- Map(function(x, y) x * y, l, w)
  combined_list <- Reduce(`+`, weighted_list)

  combined_list
}

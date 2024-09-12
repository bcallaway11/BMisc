#' @title Balance a Panel Data Set
#'
#' @description This function drops observations from data.frame
#'  that are not part of balanced panel data set.
#'
#' @param data data.frame used in function
#' @param idname unique id
#' @param tname time period name
#' @param return_data.table if TRUE, makeBalancedPanel will
#'  return a data.table rather than a data.frame.  Default
#'  is FALSE.
#' @examples
#' id <- rep(seq(1, 100), each = 2) # individual ids for setting up a two period panel
#' t <- rep(seq(1, 2), 100) # time periods
#' y <- rnorm(200) # outcomes
#' dta <- data.frame(id = id, t = t, y = y) # make into data frame
#' dta <- dta[-7, ] # drop the 7th row from the dataset (which creates an unbalanced panel)
#' dta <- makeBalancedPanel(dta, idname = "id", tname = "t")
#'
#' @return data.frame that is a balanced panel
#' @export
makeBalancedPanel <- function(data,
                              idname,
                              tname,
                              return_data.table = FALSE) {
  if (!inherits(data, "data.frame")) {
    stop("data must be a data.frame")
  }

  data.table::setDT(data)

  nt <- length(unique(data[[tname]]))
  if (!return_data.table) {
    return(as.data.frame(data[, if (.N == nt) .SD, by = idname]))
  } else if (return_data.table) {
    return(data[, if (.N == nt) .SD, by = idname])
  }
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
#' @return data.frame
#' @export
panel2cs <- function(data, timevars, idname, tname) {
  # .Deprecated("panel2cs2")

  if (length(unique(data[, tname])) != 2) {
    stop("panel2cs only for 2 periods of panel data")
  }

  # balance the data, just in case
  data <- makeBalancedPanel(data, idname, tname)

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
#' @return data from first period with .y0 (outcome in first period),
#'  .y1 (outcome in second period), and .dy (change in outcomes
#'  over time) appended to it
#' @export
panel2cs2 <- function(data, yname, idname, tname, balance_panel = TRUE) {
  # check that only 2 periods of data
  if (length(unique(data[[tname]])) != 2) {
    stop("panel2cs only for 2 periods of panel data")
  }

  # balance the data, just in case
  if (balance_panel) {
    data <- makeBalancedPanel(data, idname, tname)
  }

  # data.table sorting (fast and memory efficient)
  data.table::setDT(data)
  data.table::setorderv(data, cols = c(idname, tname))

  # Trick to speed up by specializing for task at hand
  # relies on being sorted by tname above
  data$.y1 <- data.table::shift(data[[yname]], -1)
  data$.y0 <- data[[yname]]
  data$.dy <- data$.y1 - data$.y0

  # Subset to first row
  first.period <- min(data[[tname]])
  data <- data[data[[tname]] == first.period, ]

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
#' bbs <- blockBootSample(LaborSupply, "id")
#' nrow(bbs)
#' head(bbs$id)
#'
#' @export
blockBootSample <- function(data, idname) {
  n <- nrow(data)
  ids <- sample(unique(data[, idname]), replace = TRUE)
  newid <- seq(1:length(ids))
  b1 <- lapply(1:length(ids), function(i) {
    bd <- data[data[, idname] == ids[i], ]
    bd[, idname] <- newid[i]
    bd
  })
  do.call(rbind, b1)
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
#' F <- makeDist(y, u)
#'
#' @return ecdf
#' @export
makeDist <- function(x, Fx, sorted = FALSE, rearrange = FALSE, force01 = FALSE, method = "constant") {
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
  class(retF) <- c("ecdf", "stepfun", class(retF))
  assign("nobs", length(x), envir = environment(retF))
  retF
}


#' @title Invert Ecdf
#'
#' @description take an ecdf object and invert it to get a step-quantile
#'  function
#'
#' @param df an ecdf object
#'
#' @return stepfun object that contains the quantiles of the df
#'
#' @export
invertEcdf <- function(df) {
  q <- knots(df)
  tau <- df(q)
  q <- c(q[1], q)
  stepfun(tau, q)
}

## ## TODO: fix this, can reference quantreg package
## ecdf2density <- function(df) {
##     q <- knots(df)
##     tau <- df(q)
##     ## akjfun comes from rq package
##     akjfun <- function(z, p, d = 10, g = 300, ...) {
##         mz <- sum(z * p)
##         sz <- sqrt(sum((z - mz)^2 * p))
##         hz <- seq(mz - d * sz, mz + d * sz, length = g)
##         fz <- quantreg::akj(z, hz, p = p, ...)$dens
##         approxfun(hz, fz)
##     }
##     p <- diff(taus)
##     akjfun(q, p)
## }




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
#' @return numeric
#' @export
weighted.checkfun <- function(q, cvec, tau, weights) {
  w <- weights
  retval <- mean(w * checkfun(cvec - q, tau))
  return(retval)
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
getWeightedQuantile <- function(tau, cvec, weights = NULL, norm = TRUE) {
  if (is.null(weights)) {
    weights <- 1
  }
  mw <- mean(weights)
  if (norm) {
    weights <- weights / mw
  }
  return(optimize(weighted.checkfun,
    lower = min(cvec),
    upper = max(cvec),
    cvec = cvec, tau = tau, weights = weights
  )$minimum)
}

#' @title Get Weighted Quantiles
#'
#' @description Finds multiple quantiles by repeatedly calling
#'  getWeightedQuantile
#'
#' @param tau a vector of values between 0 and 1
#' @param cvec a vector to compute quantiles for
#' @param weights the weights, weighted.checkfun normalizes the weights
#'  to sum to 1.
#' @param norm normalize the weights so that they have mean of 1, default is
#'  to normalize
#'
#' @return vector of quantiles
#' @export
getWeightedQuantiles <- function(tau, cvec, weights = NULL, norm = TRUE) {
  vapply(tau, getWeightedQuantile, 1.0, cvec = cvec, weights = weights, norm = norm)
  ## wtd.quantile(cvec, weights=weights, probs=tau, normwt=T)
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
#' @return the weighted mean
#' @export
getWeightedMean <- function(y, weights = NULL, norm = TRUE) {
  if (is.null(weights)) {
    weights <- 1
  }
  mw <- mean(weights)
  if (norm) {
    weights <- weights / mw
  }
  mean(weights * y)
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
#' @return ecdf
#' @export
getWeightedDf <- function(y, y.seq = NULL, weights = NULL, norm = TRUE) {
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
  dvals <- vapply(y.seq, FUN = function(x) {
    mean(weights * (y <= x))
  }, 1.0)
  makeDist(y.seq, dvals)
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
#' @return the change in outcomes over time
#' @export
cs2panel <- function(cs1, cs2, yname) {
  nu <- min(nrow(cs2), nrow(cs2))
  if (nu == nrow(cs2)) {
    ut <- cs2[, yname]
    ut <- ut[order(-ut)] ## orders largest to smallest
    ps <- seq(1, 0, length.out = length(ut)) ## orders largest to smallest
    utmin1 <- quantile(cs1[, yname], probs = ps, type = 1)
    ## F.untreated.change.t <- ecdf(ut-utmin1)
  } else {
    utmin1 <- cs2[, yname]
    utmin1 <- utmin1[order(-utmin1)] ## orders largest to smallest
    ps <- seq(1, 0, length.out = length(utmin1)) ## orders largest to smallest
    ut <- quantile(cs1[, yname], probs = ps, type = 1)
    ## F.untreated.change.t <- ecdf(ut-utmin1)
  }
  return(ut - utmin1)
}




#' @title Compare Variables across Groups
#'
#' @description \code{compareBinary} takes in a variable e.g. union
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
#' @return matrix of results
#' @export
compareBinary <- function(x, on, dta, w = rep(1, nrow(dta)), report = c("diff", "levels", "both")) {
  if (inherits(dta[, x], "factor")) {
    df <- model.matrix(as.formula(paste0("~", x, "-1")), dta)
    vnames <- colnames(df)
    df <- data.frame(cbind(df, dta[, on]))
    colnames(df) <- c(vnames, "treat")
    t(simplify2array(lapply(vnames, compareSingleBinary, on = "treat", dta = df, w = w, report = report)))
  } else {
    compareSingleBinary(x, on, dta, w, report)
  }
}

#' @title Compare a single variable across two groups
#'
#' @description \code{compareBinary} takes in a variable e.g. union
#' and runs bivariate regression of x on treatment (for summary statistics)
#'
#' @inheritParams compareBinary
#'
#' @return matrix of results
#'
#' @keywords internal
compareSingleBinary <- function(x, on, dta, w = rep(1, nrow(dta)), report = c("diff", "levels", "both")) {
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
#' @param formla a formula
#'
#' @examples
#' ff <- yvar ~ x1 + x2
#' rhs.vars(ff)
#'
#' ff <- y ~ x1 + I(x1^2)
#' rhs.vars(ff)
#'
#' @return vector of variable names
#' @export
rhs.vars <- function(formla) {
  ## allvars <- all.vars(formla)
  ## if (length(formla)==3) {
  ##   allvars <- allvars[-1]
  ## }
  labels(terms(formla))
}

#' @title Left-hand Side Variables
#'
#' @description Take a formula and return a vector of the variables
#'  on the left hand side, it will return NULL for a one sided formula
#'
#' @inheritParams rhs.vars
#'
#' @examples
#' ff <- yvar ~ x1 + x2
#' lhs.vars(ff)
#' @return vector of variable names
#' @export
lhs.vars <- function(formla) {
  if (length(formla) == 2) {
    return(NULL) ## there is no lhs variable
  }
  all.vars(formla)[1]
}

#' @title Right-hand Side of Formula
#'
#' @description Take a formula and return the right hand side
#'  of the formula
#'
#' @param formla a formula
#'
#' @examples
#' ff <- yvar ~ x1 + x2
#' rhs(ff)
#'
#' @return a one sided formula
#' @export
rhs <- function(formla) {
  toformula(NULL, rhs.vars(formla))
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
#' toformula("yvar", rhs.vars(~1))
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
#' @description \code{addCovFromFormla} adds some covariates to a formula;
#'   covs should be a list of variable names
#'
#'
#' @param covs should be a list of variable names
#' @param formla which formula to add covariates to
#' @return formula
#'
#' @examples
#' formla <- y ~ x
#' addCovToFormla(list("w", "z"), formla)
#'
#' formla <- ~x
#' addCovToFormla("z", formla)
#'
#' @export
addCovToFormla <- function(covs, formla) {
  vs <- rhs.vars(formla) ## vector of x variable names
  vs <- c(vs, covs)
  formla <- toformula(lhs.vars(formla), vs)
  return(formla)
}


#' @title Drop a Covariate from a Formula
#' @description \code{dropCovFromFormla} adds drops some covariates from a
#' formula; covs should be a list of variable names
#'
#'
#' @param covs should be a list of variable names
#' @param formla which formula to drop covariates from
#' @return formula
#'
#' @examples
#' formla <- y ~ x + w + z
#' dropCovFromFormla(list("w", "z"), formla)
#'
#' dropCovFromFormla("z", formla)
#'
#' @export
dropCovFromFormla <- function(covs, formla) {
  vs <- rhs.vars(formla)
  vs <- vs[!(vs %in% covs)]
  toformula(lhs.vars(formla), vs)
}





#' @title Combine Two Distribution Functions
#'
#' @description Combines two distribution functions with given weights by pstrat
#' @param y.seq sequence of possible y values
#' @param dflist list of distribution functions to combine
#' @param pstrat a vector of weights to put on each distribution function;
#'  if weights are not provided then equal weight is given to each
#'  distribution function
#' @param ... additional arguments that can be past to BMisc::makeDist
#'
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100, 1, 1)
#' Fx <- ecdf(x)
#' Fy <- ecdf(y)
#' both <- combineDfs(seq(-2, 3, 0.1), list(Fx, Fy))
#' plot(Fx, col = "green")
#' plot(Fy, col = "blue", add = TRUE)
#' plot(both, add = TRUE)
#'
#' @return ecdf
#' @export
combineDfs <- function(y.seq, dflist, pstrat = NULL, ...) {
  if (is.null(pstrat)) {
    pstrat <- rep(1 / length(dflist), length(dflist))
  }
  y.seq <- y.seq[order(y.seq)]
  df.valslist <- lapply(dflist, function(ddff) {
    ddff(y.seq)
  })
  df.valsmat <- simplify2array(df.valslist)
  for (i in 1:length(pstrat)) {
    df.valsmat[, i] <- df.valsmat[, i] * pstrat[i]
  }

  df.vals <- rowSums(df.valsmat)

  makeDist(y.seq, df.vals, ...)
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
## THESE ARE THROWING ERRORS

## this should return the distribution function
## currently running ci.qte and then inverting, but probably
## would be better to calculate it directly
## ci.treated.Df <- function(data, y.seq, formla, xformla, probs, weights=NULL, se, iters, retEachIter, method="logit", pl, cores) {

##     qp <- QTEparams(formla, xformla, t=NULL, tmin1=NULL, tmin2=NULL, tname=NULL, data=data, weights=weights, idname=NULL, probs=probs, iters=iters, alp=alp, method=method, plot=plot, se=se, retEachIter=retEachIter, bootstrapiter=FALSE, seedvec=NULL, pl=pl, cores=cores)
##     setupData(qp)

##     pscore.reg <- glm(data[,treat] ~ as.matrix(data[,x]),
##                       family=binomial(link=method))
##     pscore <- fitted(pscore.reg)
##     d <- data[,treat]
##     y <- data[,yname]

##     y.seq <- y.seq[order(y.seq)]
##     df.vals <- vapply(y.seq, function(x) {
##         mean((d/pscore)*(y <= x) / (mean(d/pscore))) }, 1.0)
##     makeDist(y.seq, df.vals)
## }

## ## this should return the distribution function
## ## currently running ci.qte and then inverting, but probably
## ## would be better to calculate it directly
## ci.untreated.Df <- function(data, y.seq, formla, xformla, probs, weights=NULL, se, iters, retEachIter, method="logit", pl, cores) {
##     ##OLD: using qte method
##     ##cfirp <- ci.qte(formla=formla, xformla=xformla,
##     ##            probs=probs, weights=weights, se=se, iters=iters,
##     ##            retEachIter=RE, pl=pl, cores=cores, data=data)
##     ##list(cfirp$F.treated.t, cfirp$F.treated.t.cf)
##     qp <- QTEparams(formla, xformla, t=NULL, tmin1=NULL, tmin2=NULL, tname=NULL, data=data, weights=weights, idname=NULL, probs=probs, iters=iters, alp=alp, method=method, plot=plot, se=se, retEachIter=retEachIter, bootstrapiter=FALSE, seedvec=NULL, pl=pl, cores=cores)
##     setupData(qp)

##     pscore.reg <- glm(data[,treat] ~ as.matrix(data[,x]),
##                       family=binomial(link=method))
##     pscore <- fitted(pscore.reg)
##     d <- data[,treat]
##     y <- data[,yname]
##     y.seq <- y.seq[order(y.seq)]
##     df.vals <- vapply(y.seq, function(x) {
##         mean(((1-d)/(1-pscore))*(y <= x) / mean((1-d)/(1-pscore))) }, 1.0)
##     makeDist(y.seq, df.vals)
## }

## ##get the distribution function
## ## under stratified random sampling
## strat.ci.df <- function(y.seq, stratvarname, pstrat, formla, xformla, data, probs, weights, se, iters, retEachIter, pl, cores) {

##     browser()

##     cdta <- lapply(unique(data[,stratvarname]),
##                    function(x) { data[data[,stratvarname]==x,] })
##     ctreatedflist <- lapply(cdta, ci.treated.Df, y.seq=y.seq, formla=formla,
##                         xformla=xformla, probs=probs,
##                         se=se, iters=iters, retEachIter=retEachIter,
##                         pl=pl, cores=cores)
##     treated.df <- combineDfs(y.seq, ctreatedflist, pstrat)
##     cuntreatedflist <- lapply(cdta, ci.untreated.Df, y.seq=y.seq, formla=formla,
##                         xformla=xformla, probs=probs,
##                         se=se, iters=iters, retEachIter=retEachIter,
##                         pl=pl, cores=cores)
##     untreated.df <- combineDfs(y.seq, cuntreatedflist, pstrat)
##     return(c(treated.df, untreated.df))
## }


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
#' getListElement(lis, "x")[1] # should be equal to -1
#' getListElement(lis, 1)[1] # should be equal to -1
#'
#' @export
getListElement <- function(listolists, whichone = 1) {
  lapply(listolists, function(l) l[[whichone]])
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
#' @return logical vector
#'
#' @export
TorF <- function(cond, use_isTRUE = FALSE) {
  if (!is.logical(cond)) stop("cond should be a logical vector")

  if (use_isTRUE) {
    cond <- sapply(cond, isTRUE)
  } else {
    cond[is.na(cond)] <- FALSE
  }
  cond
}


#' @title get_group_inner
#' @description Calculates the group for a particular unit
#' @param this_df a data.frame, for this function it should be specific to
#'  a particular unit
#' @inheritParams get_group
#' @keywords internal
#' @export
get_group_inner <- function(this_df, tname, treatname) {
  if (all(this_df[, treatname] == 0)) {
    return(0)
  }

  as.numeric(this_df[this_df[, treatname] > 0, ][1, tname])
}

#' @title get_group
#' @description A function to calculate a unit's group in a panel data setting
#'  with a binary treatment and staggered treatment adoption and where
#'  there is a column in the data indicating whether or not a unit is treated
#' @param df the data.frame used in the function
#' @param idname name of column that holds the unit id
#' @param tname name of column that holds the time period
#' @param treatname name of column with the treatment indicator
#' @export
get_group <- function(df, idname, tname, treatname) {
  group_vec <- df %>%
    group_by(.data[[idname]]) %>%
    group_map(~ rep(get_group_inner(.x, tname, treatname), nrow(.x))) %>%
    unlist()
  group_vec
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
#' @export
get_YiGmin1 <- function(df, idname, yname, tname, gname) {
  YiGmin1_vec <- df %>%
    group_by(.data[[idname]]) %>%
    group_map(~ rep(get_YiGmin1_inner(.x, yname, tname, gname), nrow(.x))) %>%
    unlist()
  YiGmin1_vec
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
#' @export
get_Yi1 <- function(df, idname, yname, tname, gname) {
  Yi1_vec <- df %>%
    group_by(.data[[idname]]) %>%
    group_map(~ rep(get_Yi1_inner(.x, yname, tname, gname), nrow(.x))) %>%
    unlist()
  Yi1_vec
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
#' @return a vector of outcomes in period t, the vector
#'  will have the length nT (i.e., this is returned for
#'  each element in the panel, not for a particular period)
#' @export
get_Yit <- function(df, tp, idname, yname, tname) {
  Yit_vec <- df %>%
    group_by(.data[[idname]]) %>%
    group_map(~ rep(get_Yit_inner(.x, tp, yname, tname), nrow(.x))) %>%
    unlist()
  Yit_vec
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
#' @export
get_Yibar <- function(df, idname, yname) {
  Yibar_vec <- df %>%
    group_by(.data[[idname]]) %>%
    group_map(~ rep(get_Yibar_inner(.x, yname), nrow(.x))) %>%
    unlist()
  Yibar_vec
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
  maxT <- max(this_df[, tname])
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
#' @export
get_Yibar_pre <- function(df, idname, yname, tname, gname) {
  YiGmin1_vec <- df %>%
    group_by(.data[[idname]]) %>%
    group_map(~ rep(get_Yibar_pre_inner(.x, yname, tname, gname), nrow(.x))) %>%
    unlist()
  YiGmin1_vec
}

#' @title get_lagYi
#' @description A function that calculates lagged outcomes in a panel data setting.
#'  If the data.frame that is passed in has nxT rows, the resulting vector will
#'  also have nxT elements with one element for each unit set to be NA
#' @inheritParams get_Yi1
#' @param nlags The number of periods to lag.  The default is 1, which computes
#'  the lag from the previous period.
#' @export
get_lagYi <- function(df, idname, yname, tname, nlags = 1) {
  df <- df %>%
    dplyr::group_by(.data[[idname]]) %>%
    dplyr::mutate(.lag = dplyr::lag(.data[[yname]], nlags, order_by = .data[[tname]]))
  df$.lag
}

#' @title get_first_difference
#' @description A function that calculates the first difference in a panel data
#'  setting.  If the data.frame that is passed in has nxT rows, the resulting
#'  vector will also have nxT elements with one element for each unit set to be
#'  NA.
#' @inheritParams get_lagYi
#' @export
get_first_difference <- function(df, idname, yname, tname) {
  df$.lag <- get_lagYi(df, idname, yname, tname)
  df[, yname] - df$.lag
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
#' @description A helper function to check if treatment is staggered in a panel data set.
#'
#' @inheritParams get_group_inner
#'
#' @keywords internal
#' @export
check_staggered_inner <- function(this_df, treatname) {
  this_df <- as.data.frame(this_df)
  is_staggered <- TRUE
  if (length(unique(this_df[, treatname])) > 1) is_staggered <- FALSE
  if (any(diff(this_df[, treatname]) < 0)) is_staggered <- FALSE
  is_staggered
}

#' @title check_staggered
#'
#' @description A function to check if treatment is staggered in a panel data set.
#'
#' @inheritParams get_group
#'
#' @return a logical indicating whether treatment is staggered
#' @export
check_staggered <- function(df, idname, treatname) {
  this_staggered <- df %>%
    group_by(.data[[idname]]) %>%
    group_map(~ check_staggered_inner(.x, treatname)) %>%
    unlist()
  all(this_staggered)
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
#' @return original time period converted from new time period
#'
#' @export
t2orig <- function(t, original_time.periods) {
  # check that orignal time periods are equally spaced
  if (length(unique(diff(original_time.periods))) > 1) {
    warning("original_time.periods are unequally spaced, some downstream functions may not work as expected.")
  }
  sapply(t, t2orig_inner, original_time.periods = original_time.periods)
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
#' @return new time period converted from original time period
#'
#' @export
orig2t <- function(orig, original_time.periods) {
  # check that orignal time periods are equally spaced
  if (length(unique(diff(original_time.periods))) > 1) {
    warning("original_time.periods are unequally spaced, some downstream functions may not work as expected.")
  }
  sapply(orig, orig2t_inner, original_time.periods = original_time.periods)
}

#' @title drop_collinear
#' @description A function to check for multicollinearity and drop collinear terms
#'  from a matrix
#' @param matrix a matrix for which the function will remove collinear columns
#' @return a matrix with collinear columns removed
#' @export
drop_collinear <- function(matrix) {
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
#'  is equal to n = length(unique(data[[idname]])) or long format (where the number
#'  of rows is equal to nT = nrow(data)).  The default is FALSE, so that long data
#'  is returned by default.
#' @param ret_id whether to return the id column in the output data.frame.  The default is FALSE.
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
  for (i in 1:ncol(X)) {
    this_x_name <- colnames(X)[i]
    x <- X[, i]
    df <- data.frame(.id = data[[idname]], .time = data[[tname]], x)
    wide_data <- df %>% pivot_wider(id_cols = .id, names_from = .time, names_prefix = "_x_", values_from = x)
    .id <- wide_data$.id
    pca_inner <- wide_data %>%
      select(starts_with("_x_")) %>%
      prcomp(center = FALSE, scale. = FALSE)
    princ_comp <- pca_inner$x[, 1:n_components]
    colnames(princ_comp) <- paste0(this_x_name, "_", colnames(princ_comp))
    pc_list[[i]] <- princ_comp
  }
  pc_data <- do.call(cbind.data.frame, pc_list)
  if (ret_id) {
    pc_data <- cbind.data.frame(.id, pc_data)
  }

  if (ret_wide) {
    return(pc_data)
  } else {
    return(pc_data[rep(1:nrow(pc_data), each = nperiods), ])
  }
}

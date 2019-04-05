###makeBalancedPanel is a function to take a dataset
## and make sure that all years are available for 
## all observations.  If some years are not available,
## then that observation is dropped.
#'@title makeBalancedPanel
#' 
#' @description This function drops observations from data.frame
#'  that are not part of balanced panel data set.
#' 
#' @param data data.frame used in function
#' @param idname unique id
#' @param tname time period name
#' @examples
#' id <- rep(seq(1,100,1),2) ## individual ids for setting up a two period panel
#' t <- rep(seq(1,2),100) ## time periods
#' y <- rnorm(200) ## outcomes
#' dta <- data.frame(id=id, t=t, y=y) ## make into data frame
#' dta <- dta[-7,] ## drop the 7th row from the dataset (which creates an unbalanced panel)
#' dta <- makeBalancedPanel(dta, idname="id", tname="t")
#' 
#' @return data.frame that is a balanced panel
#' @export
makeBalancedPanel <- function(data, idname, tname) {
    nt <- length(unique(data[,tname]))
    agg <- aggregate(data[,idname], by=list(data[,idname]), length)
    rightids <- agg[,1][agg[,2]==nt]
    bp <- data[data[,idname] %in% rightids,]
    return(bp)
}


#'@title panel2cs
#' 
#' @description panel2cs takes a 2 period dataset and turns it
#'  into a cross sectional dataset.  The default functionality
#'  is to keep all the variables from period 1
#'  and add all the variables listed by name in timevars
#'  from period 2 to those
#' 
#' @param data data.frame used in function
#' @param timevars vector of names of variables to keep
#' @param idname unique id
#' @param tname time period name
#' 
#' @return data.frame
#' @export
panel2cs <- function(data, timevars, idname, tname) {

    if (length(unique(data[,tname])) != 2) {
        stop("panel2cs only for 2 periods of panel data")
    }

    data <- makeBalancedPanel(data, idname, tname) ## just in case
    data <- data[order(data[,idname], data[,tname]),] ## put everything in the right order
    ## so we can match it easily later on

    tdta <- aggregate(data[,timevars], by=list(data[,idname]), FUN=function(x) { x[2] })

    t1 <- unique(data[,tname])
    t1 <- t1[order(t1)][1]
    retdat <- subset(data, data[,tname]==t1)
    retdat$yt1 <- tdta[,2]
    retdat$dy <- retdat$yt1 - retdat$y
    return(retdat)
}

#'@title ids2rownum
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
#' ids <- seq(1,1000,length.out=100)
#' ids <- ids[order(runif(100))]
#' df <- data.frame(id=ids)
#' ids2rownum(df$id, df, "id")
#' 
#' @return vector of row numbers
#' @export
ids2rownum <- function(ids, data, idname) {
    vapply(ids, id2rownum, 1.0, data=data, idname=idname)
}


#'@title ids2rownum
#' 
#' @description id2rownum takes an id and converts it t the right
#'  row number in the dataset; ids should be unique in the dataset
#'  that is, don't pass the function panel data with multiple same ids
#' 
#' @param ids vector of ids
#' @param data data frame
#' @param idname unique id
#' 
#' @keywords internal
id2rownum <- function(id, data, idname) {
    which(data[,idname] == id)
}

#' @title blockBootSample
#'
#' @description make draws of all observations with the same id in a panel
#'  data context.  This is useful for bootstrapping with panel data.
#'
#' @param data data.frame from which you want to bootstrap
#' @param idname column in data which contains an individual identifier
#'
#' @return data.frame bootstrapped from the original dataset; this data.frame
#'  will contain new ids
#' @export
blockBootSample <- function(data, idname) {
    n <- nrow(data)
    ids <- sample(unique(data[,idname]), replace=TRUE)
    newid <- seq(1:length(ids))
    b1 <- lapply(1:length(ids), function(i) {
        bd <- data[ data[,idname]==ids[i],]
        bd[,idname] <- newid[i]
        bd
    })
    do.call(rbind, b1)
}



#'@title makeDist
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
#' @examples
#' y <- rnorm(100)
#' y <- y[order(y)]
#' u <- runif(100)
#' u <- u[order(u)]
#' F <- makeDist(y,u)
#' 
#' @return ecdf
#' @export
makeDist <- function(x, Fx, sorted=FALSE, rearrange=FALSE) {
    if (!sorted) {
        tmat <- cbind(x, Fx)
        tmat <- tmat[order(x),]
        x <- tmat[,1]
        Fx <- tmat[,2]
    }

    if (rearrange) {
        Fx <- sort(Fx)
    }
    
    retF <- approxfun(x, Fx, method="constant",
                      yleft=0, yright=1, f=0, ties="ordered")
    class(retF) <- c("ecdf", "stepfun", class(retF))
    assign("nobs", length(x), envir = environment(retF))
    retF
}


#' @title invertEcdf
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
    



#'@title checkfun
#' 
#' @description The check function used for optimizing to get quantiles
#' 
#' @param a vector to compute quantiles for
#' @param tau between 0 and 1, ex. .5 implies get the median
#'
#' @examples
#' x <- rnorm(100)
#' x[which.min(checkfun(x, 0.5))] ##should be around 0
#' 
#' @return numeric value
#' @export
checkfun <- function(a, tau) {
    return(a*(tau - (1*(a<=0))))
}

#'@title weighted.checkfun
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
weighted.checkfun = function(q, cvec, tau, weights) {
    w <- weights
    retval <- mean(w*checkfun(cvec-q,tau))
    return(retval)
}


#'@title getWeightedQuantile
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
getWeightedQuantile <- function(tau, cvec, weights=NULL, norm=TRUE) {
    if (is.null(weights)) {
        weights <- 1
    }
    mw <- mean(weights)
    if (norm) {
        weights <- weights / mw
    }
    return(optimize(weighted.checkfun, 
                    lower=min(cvec),
                    upper=max(cvec),
                    cvec=cvec, tau=tau, weights=weights)$minimum)
}

#'@title getWeightedQuantiles
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
getWeightedQuantiles <- function(tau, cvec, weights=NULL, norm=TRUE) {
    vapply(tau, getWeightedQuantile, 1.0, cvec=cvec, weights=weights, norm=norm)
    ##wtd.quantile(cvec, weights=weights, probs=tau, normwt=T)
}

#'@title getWeightedMean
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
getWeightedMean <- function(y, weights=NULL, norm=TRUE) {
    if (is.null(weights)) {
        weights <- 1
    }
    mw <- mean(weights)
    if (norm) {
        weights <- weights/mw
    }
    mean(weights*y)
}

#'@title getWeightedDf
#' 
#' @description Get the mean applying some weights
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
getWeightedDf <- function(y, y.seq=NULL, weights=NULL, norm=TRUE) {
    if (is.null(weights)) {
        weights <- 1
    }
    mw <- mean(weights)
    if (norm) {
        weights <- weights/mw
    }
    if (is.null(y.seq)) {
        y.seq <- unique(y)
        y.seq <- y.seq[order(y.seq)]
    }
    dvals <- vapply(y.seq, FUN=function(x) { mean(weights*(y <= x)) }, 1.0)
    makeDist(y.seq, dvals)
}

#' @title cs2panel
#'
#' @description Turn repeated cross sections data into panel data by imposing rank invariance; does not
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
        ut <- cs2[,yname]
        ut <- ut[order(-ut)] ##orders largest to smallest
        ps <- seq(1,0,length.out=length(ut)) ##orders largest to smallest
        utmin1 <- quantile(cs1[,yname], probs=ps, type=1)
        ##F.untreated.change.t <- ecdf(ut-utmin1)
    } else {
        utmin1 <- cs2[,yname]
        utmin1 <- utmin1[order(-utmin1)] ##orders largest to smallest
        ps <- seq(1,0,length.out=length(utmin1)) ##orders largest to smallest
        ut <- quantile(cs1[,yname], probs=ps, type=1)
        ##F.untreated.change.t <- ecdf(ut-utmin1)
    }
    return(ut - utmin1)
}




#' @title compareBinary
#'
#' @description \code{compareBinary} ##takes in a variable e.g. union
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
compareBinary <- function(x, on, dta, w=rep(1,nrow(dta)), report=c("diff","levels","both")) {
    if (class(dta[,x]) == "factor") {
        df <- model.matrix(as.formula(paste0("~",x,"-1")), dta)
        vnames <- colnames(df)
        df <- data.frame(cbind(df, dta[,on]))
        colnames(df) <- c(vnames, "treat")
        t(simplify2array(lapply(vnames, compareSingleBinary, on="treat", dta=df, w=w, report=report)))
    } else {
        compareSingleBinary(x, on, dta, w, report)
    }
}

#' @title compareSingleBinary
#'
#' @description \code{compareBinary} ##takes in a variable e.g. union
#' and runs bivariate regression of x on treatment (for summary statistics)
#'
#' @inheritParams compareBinary
#' 
#' @return matrix of results
#' 
#' @keywords internal
compareSingleBinary <- function(x, on, dta, w=rep(1,nrow(dta)), report=c("diff","levels","both")) {
    coefmat <- summary(lm(as.formula(paste(x, on ,sep=" ~ ")), data=dta,
                          weights=w))$coefficients
    if (report=="diff") {
        return(c(coefmat[1,1] + coefmat[2,1], coefmat[1,1], abs(coefmat[2,3])>1.96))
    } else if (report=="levels") { ## report the levels
        return(c(coefmat[1,1] + coefmat[2,1], coefmat[1,1], abs(coefmat[2,3])>1.96))
    } else if (report=="both") {
        return(c(coefmat[1,1] + coefmat[2,1], coefmat[1,1], coefmat[2,1], round(coefmat[2,4],3)))
    }
}

##-------------------------------------------------------##
##########################################################|
##                                                        |
###################### HELPER FUNCTIONS ##################|

#' @title rhs.vars
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

#' @title lhs.vars
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

#' @title rhs
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
  toformula(NULL,rhs.vars(formla))
}

#' @title toformula
#' 
#' @description take a name for a y variable and a vector of names
#'  for x variables and turn them into a formula
#'
#' @param yname the name of the y variable
#' @param xnames vector of names for x variables
#'
#' @examples
#' toformula("yvar", c("x1","x2"))
#'
#' ## should return yvar ~ 1
#' toformula("yvar", rhs.vars(~1))
#'
#' @return a formula
#' @export
toformula <- function(yname, xnames) {
    if (length(xnames)==0) {
        return(as.formula(paste0(yname," ~ 1")))
    }
    out <- paste0(yname,"~")
    xpart <- paste0(xnames, collapse="+")
    out <- paste0(out,xpart)
    out <- as.formula(out)
    out
}

#' @title addCovToFormla
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
#' addCovToFormla(list("w","z"), formla)
#'
#' formla <- ~x
#' addCovToFormla("z", formla)
#' 
#' @export
addCovToFormla <- function(covs, formla) {
    vs <- rhs.vars(formla) ## vector of x variable names
    vs <- c(vs, covs)
    formla <- toformula(lhs.vars(formla), vs)
    # newformla <- paste(vs, collapse="+")
    # newformla <- paste("~", newformla)
    # newformla <- as.formula(newformla)
    # leftside <- formula.tools::lhs(formla) ## seems like bug in formula.tools is reason why need to do this
    # formula.tools::rhs(formla) <- formula.tools::rhs(newformla)
    # formula.tools::lhs(formla) <- leftside
    return(formla)
}


##add some covariates to a formula
##covs should be a list of variable names
#' @title dropCovFromFormla
#' @description \code{dropCovFromFormla} adds drops some covariates from aformula;
#'   covs should be a list of variable names
#'
#' 
#' @param covs should be a list of variable names
#' @param formla which formula to drop covariates from
#' @return formula
#'
#' @examples
#' formla <- y ~ x + w + z
#' dropCovFromFormla(list("w","z"), formla)
#'
#' dropCovFromFormla("z", formla)
#' 
#' @export
dropCovFromFormla <- function(covs, formla) {
    vs <- rhs.vars(formla)
    vs <- vs[!(vs %in% covs)]
    toformula(lhs.vars(formla), vs)
    ## vs <- formula.tools::rhs.vars(formla) ## vector of x variable names
    ## vs <- vs[!(vs %in% covs)]
    ## newformla <- paste(vs, collapse="+")
    ## newformla <- paste("~", newformla)
    ## newformla <- as.formula(newformla)
    ## formula.tools::rhs(formla) <- formula.tools::rhs(newformla)
    ## return(formla)
}





#'@title combineDfs
#' 
#' @description Combines two distribution functions with given weights by pstrat
#' @param y.seq sequence of possible y values
#' @param dflist list of distribution functions to combine
#' @param pstrat a vector of weights to put on each distribution function;
#'  if weights are not provided then equal weight is given to each
#'  distribution function
#'
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100,1,1)
#' Fx <- ecdf(x)
#' Fy <- ecdf(y)
#' both <- combineDfs(seq(-2,3,0.1), list(Fx,Fy))
#' plot(Fx, col="green")
#' plot(Fy, col="blue", add=TRUE)
#' plot(both, add=TRUE)
#' 
#' @return ecdf
#' @export
combineDfs <- function(y.seq, dflist, pstrat=NULL) {
    if (is.null(pstrat)) {
        pstrat <- rep(1/length(dflist), length(dflist))
    }            
    y.seq <- y.seq[order(y.seq)]
    df.valslist <- lapply(dflist, function(ddff) {
        ddff(y.seq)})
    df.valsmat <- simplify2array(df.valslist)
    for (i in 1:length(pstrat)) {
        df.valsmat[,i] <- df.valsmat[,i]*pstrat[i]
    }

    df.vals <- rowSums(df.valsmat)
    
    makeDist(y.seq, df.vals)
}

#' @title subsample
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
#' data(LaborSupply, package="plm")
#' nrow(LaborSupply)
#' unique(LaborSupply$year)
#' ss <- subsample(LaborSupply, "id", "year", nkeep=100)
#' nrow(ss)
#'
#' @return a data.frame that contains a subsample of \code{dta}
#' 
#' @export
subsample <- function(dta, idname, tname, keepids=NULL, nkeep=NULL) {
    ids <- unique(dta[,idname])

    if (is.null(keepids)) {
        if (is.null(nkeep)) nkeep <- length(ids)
        keepids <- sample(ids, size=nkeep)
    }

    retdta <- dta[ dta[,idname] %in% keepids, ]
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
    
    

##
##########################################################|
##-------------------------------------------------------#




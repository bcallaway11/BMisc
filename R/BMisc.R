##takes in a variable e.g. union and runs bivariate regression
## of x on treatment (for summary statistics)
compareBinary <- function(x, on, dta, w=rep(1,nrow(dta)), report=c("diff","levels","both")) {
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

##drop some covariates from a formula
##covs should be a list of variable names
dropCovFromFormla <- function(covs, formla) {
    vs <- formula.tools::rhs.vars(formla) ## vector of x variable names
    vs <- vs[!(vs %in% covs)]
    newformla <- paste(vs, collapse="+")
    newformla <- paste("~", newformla)
    newformla <- as.formula(newformla)
    return(newformla)
    ##old
    ##return(as.formula(paste("~", paste(unlist(strsplit(as.character(formla)[2], paste(cov,"+"), fixed=T)), collapse=""))))
}


##functions to get median (or specified quantile)
##requires that qte object has that value of tau
getMedian <- function(qteobj, tau=.5) {
    which.qte <- which(qteobj$probs == tau)
    return(qteobj$qte[which.qte])
}

##functions to get median (or specified quantile) standard error
##requires that qte object has that value of tau
getMedianSE <- function(qteobj, tau=.5) {
    which.qte <- which(qteobj$probs == tau)
    return(qteobj$qte.se[which.qte])
}

##functions to get 80-20 difference or some other difference in quantiles
diffquantiles <- function(qteobj, hightau, lowtau) {
    which.highqte <- which(qteobj$probs == hightau)
    which.lowqte <- which(qteobj$probs == lowtau)
    return(qteobj$qte[which.highqte] - qteobj$qte[which.lowqte])
}


##bootstrap the difference betwen quantiles
##must be called with a qteobj with retEachIter set to true
bootse.diffquantiles <- function(qteobj, hightau, lowtau) {
    bootvals <- lapply(qteobj$eachIterList, diffquantiles, hightau, lowtau)
    se <- sd(unlist(bootvals))
}




##combines two distribution functions with given weights by pstrat
combineDfs <- function(y.seq, dflist, pstrat) {
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


##get the distribution function
## under stratified random sampling
strat.ci.df <- function(y.seq, stratvarname, pstrat, formla, xformla, data, probs, weights, se, iters, retEachIter, pl, cores) {

    browser()
    
    cdta <- lapply(unique(data[,stratvarname]),
                   function(x) { data[data[,stratvarname]==x,] })
    ctreatedflist <- lapply(cdta, ci.treated.Df, y.seq=y.seq, formla=formla,
                        xformla=xformla, probs=probs,
                        se=se, iters=iters, retEachIter=retEachIter,
                        pl=pl, cores=cores)
    treated.df <- combineDfs(y.seq, ctreatedflist, pstrat)
    cuntreatedflist <- lapply(cdta, ci.untreated.Df, y.seq=y.seq, formla=formla,
                        xformla=xformla, probs=probs,
                        se=se, iters=iters, retEachIter=retEachIter,
                        pl=pl, cores=cores)
    untreated.df <- combineDfs(y.seq, cuntreatedflist, pstrat)
    return(c(treated.df, untreated.df))
}

## this should return the distribution function
## currently running ci.qte and then inverting, but probably
## would be better to calculate it directly
ci.treated.Df <- function(data, y.seq, formla, xformla, probs, weights=NULL, se, iters, retEachIter, method="logit", pl, cores) {
    ##OLD: using qte method
    ##cfirp <- ci.qte(formla=formla, xformla=xformla,
    ##            probs=probs, weights=weights, se=se, iters=iters,
    ##            retEachIter=RE, pl=pl, cores=cores, data=data)
    ##list(cfirp$F.treated.t, cfirp$F.treated.t.cf)

    qp <- QTEparams(formla, xformla, t=NULL, tmin1=NULL, tmin2=NULL, tname=NULL, data=data, weights=weights, idname=NULL, probs=probs, iters=iters, alp=alp, method=method, plot=plot, se=se, retEachIter=retEachIter, bootstrapiter=FALSE, seedvec=NULL, pl=pl, cores=cores)
    setupData(qp)

    pscore.reg <- glm(data[,treat] ~ as.matrix(data[,x]),
                      family=binomial(link=method))
    pscore <- fitted(pscore.reg)
    d <- data[,treat]
    y <- data[,yname]

    y.seq <- y.seq[order(y.seq)]
    df.vals <- vapply(y.seq, function(x) {
        mean((d/pscore)*(y <= x) / (mean(d/pscore))) }, 1.0)
    makeDist(y.seq, df.vals)
}

## this should return the distribution function
## currently running ci.qte and then inverting, but probably
## would be better to calculate it directly
ci.untreated.Df <- function(data, y.seq, formla, xformla, probs, weights=NULL, se, iters, retEachIter, method="logit", pl, cores) {
    ##OLD: using qte method
    ##cfirp <- ci.qte(formla=formla, xformla=xformla,
    ##            probs=probs, weights=weights, se=se, iters=iters,
    ##            retEachIter=RE, pl=pl, cores=cores, data=data)
    ##list(cfirp$F.treated.t, cfirp$F.treated.t.cf)
    qp <- QTEparams(formla, xformla, t=NULL, tmin1=NULL, tmin2=NULL, tname=NULL, data=data, weights=weights, idname=NULL, probs=probs, iters=iters, alp=alp, method=method, plot=plot, se=se, retEachIter=retEachIter, bootstrapiter=FALSE, seedvec=NULL, pl=pl, cores=cores)
    setupData(qp) 

    pscore.reg <- glm(data[,treat] ~ as.matrix(data[,x]),
                      family=binomial(link=method))
    pscore <- fitted(pscore.reg)
    d <- data[,treat]
    y <- data[,yname]
    y.seq <- y.seq[order(y.seq)]
    df.vals <- vapply(y.seq, function(x) {
        mean(((1-d)/(1-pscore))*(y <= x) / mean((1-d)/(1-pscore))) }, 1.0)
    makeDist(y.seq, df.vals)
}

    
    

##
##########################################################|
##-------------------------------------------------------#



##make tables using R's texreg package
require(texreg)
qteToTexreg <- function(qteobj, tau=NULL, reportAte=T) {
    if (is.null(tau)) {
        tau <- qteobj$probs
        qte <- qteobj$qte
        qte.se <- qteobj$qte.se
        ate <- qteobj$ate
        ate.se <- qteobj$ate.se        
    } else if(!(all(tau %in% qteobj$probs))) {
        stop("Error not all tau in qte object")
    } else {
        tauloc <- vapply(tau, function(x) { which(x==qteobj$probs) }, 1.0)
        qte <- qteobj$qte[tauloc]
        qte.se <- qteobj$qte.se[tauloc]
        ate <- qteobj$ate
        ate.se <- qteobj$ate.se
    }
    if (reportAte) {
        createTexreg(c(paste(tau), "ate"),
                     c(qte, ate),
                     c(qte.se, ate.se),
                     2*pnorm(-c(abs(qte/qte.se),
                                abs(ate/ate.se))))
    } else {
        createTexreg(paste(tau),
                     qte,
                     qte.se,
                     2*pnorm(-c(abs(qte/qte.se))))
    }
}

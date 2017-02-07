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

## some functions for distribution regression

dr.inner <- function(yval, formla, data) {
    y <- lhs(formla)
    x <- rhs(formla)
    lhs(formla) <- substitute(I(y <= yval), list(y=y, yval=yval))
    outreg <- glm(formla, data, family=binomial(link=logit))
}

#'@title dr
#'
#' @description Distribution Regression
#' 
#' @param formla the regression to run
#' @param y.seq the values of y to run the regression on
#' 
dr <- function(formla, data, y.seq) {
    distreg <- lapply(y.seq, dr.inner, formla, data)
    DR(y.seq, distreg)    
}

##function to take in y0 and x and return F(y0|x)
dr.predict.inner <- function(y0, x, drobj) {
    yval <- drobj$yvals[which.min(abs(drobj$yvals-y0))]
    yidx <- which(drobj$yvals==yval)
    predict(drobj$drlist[[yidx]], newdata=x, type="response")
}

##function to take a vector of ys and single x and return vector F(y|x)
dr.predict <- function(y.seq, x, drobj) {
    vapply(y.seq, dr.predict.inner, FUN.VALUE=1.0, x=x, drobj=drobj)
}

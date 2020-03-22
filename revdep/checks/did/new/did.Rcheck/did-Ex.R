pkgname <- "did"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('did')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("expf")
### * expf

flush(stderr()); flush(stdout())

### Name: expf
### Title: expf
### Aliases: expf

### ** Examples

data(mpdta)
dta <- subset(mpdta, year==2007)
X <- model.matrix(~lpop, data=dta)
X <- expf(X, X[1,])




cleanEx()
nameEx("ggdid")
### * ggdid

flush(stderr()); flush(stdout())

### Name: ggdid
### Title: ggdid
### Aliases: ggdid

### ** Examples

## Not run: 
##D data(mpdta)
##D out <- mp.spatt(lemp ~ treat, xformla=~lpop, data=mpdta,
##D                 panel=TRUE, first.treat.name="first.treat",
##D                 idname="countyreal", tname="year",
##D                 bstrap=FALSE, se=TRUE, cband=FALSE)
##D ggdid(out)
## End(Not run)




cleanEx()
nameEx("indicator")
### * indicator

flush(stderr()); flush(stdout())

### Name: indicator
### Title: indicator
### Aliases: indicator

### ** Examples

data(mpdta)
dta <- subset(mpdta, year==2007)
X <- model.matrix(~lpop, data=dta)
X <- indicator(X, X[1,])




cleanEx()
nameEx("mp.spatt")
### * mp.spatt

flush(stderr()); flush(stdout())

### Name: mp.spatt
### Title: mp.spatt
### Aliases: mp.spatt

### ** Examples

data(mpdta)

## with covariates
out1 <- mp.spatt(lemp ~ treat, xformla=~lpop, data=mpdta,
                panel=TRUE, first.treat.name="first.treat",
                idname="countyreal", tname="year",
                bstrap=FALSE, se=TRUE, cband=FALSE)
## summarize the group-time average treatment effects
summary(out1)
## summarize the aggregated treatment effect parameters
summary(out1$aggte)

## without any covariates
out2 <- mp.spatt(lemp ~ treat, xformla=NULL, data=mpdta,
                panel=TRUE, first.treat.name="first.treat",
                idname="countyreal", tname="year",
                bstrap=FALSE, se=TRUE, cband=FALSE)
summary(out2)




cleanEx()
nameEx("mp.spatt.test")
### * mp.spatt.test

flush(stderr()); flush(stdout())

### Name: mp.spatt.test
### Title: mp.spatt.test
### Aliases: mp.spatt.test

### ** Examples

## Not run: 
##D data(mpdta)
##D mptest <- mp.spatt.test(lemp ~ treat, xformlalist=list(~lpop), data=mpdta,
##D                 panel=TRUE, first.treat.name="first.treat",
##D                 idname="countyreal", tname="year", clustervarlist=list(NULL))
##D summary(mptest[[1]])
## End(Not run)

data(mpdta)
mptest <- mp.spatt.test(lemp ~ treat, xformlalist=list(NULL), data=mpdta,
                panel=TRUE, first.treat.name="first.treat",
                idname="countyreal", tname="year", clustervarlist=list(NULL))
summary(mptest[[1]])




cleanEx()
nameEx("onefun")
### * onefun

flush(stderr()); flush(stdout())

### Name: onefun
### Title: onefun
### Aliases: onefun

### ** Examples

data(mpdta)
dta <- subset(mpdta, year==2007)
X <- model.matrix(~lpop, data=dta)
X <- onefun(X, X[1,])




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

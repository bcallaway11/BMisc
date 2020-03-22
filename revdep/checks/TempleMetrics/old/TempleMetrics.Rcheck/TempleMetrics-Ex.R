pkgname <- "TempleMetrics"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('TempleMetrics')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Fycondx.DR")
### * Fycondx.DR

flush(stderr()); flush(stdout())

### Name: Fycondx.DR
### Title: Fycondx.DR
### Aliases: Fycondx.DR

### ** Examples

data(igm)
yvals <- seq(quantile(igm$lcfincome,.05,type=1),
 quantile(igm$lcfincome,.95, type=1), length.out=100)
dres <- distreg(lcfincome ~ lfincome + HEDUC, igm, yvals)
xdf <- data.frame(lfincome=10, HEDUC=c("LessHS","HS"))
d <- Fycondx(dres, yvals, xdf)
d
y0 <- yvals[50]
d[[1]](y0)




cleanEx()
nameEx("distreg")
### * distreg

flush(stderr()); flush(stdout())

### Name: distreg
### Title: distreg
### Aliases: distreg

### ** Examples

data(igm)
y0 <- median(igm$lcfincome)
distreg(lcfincome ~ lfincome + HEDUC, igm, y0)




cleanEx()
nameEx("lldistreg")
### * lldistreg

flush(stderr()); flush(stdout())

### Name: lldistreg
### Title: lldistreg
### Aliases: lldistreg

### ** Examples

data(igm)
lldistreg(lcfincome ~ lfincome, ~HEDUC, igm, 10, 10)




cleanEx()
nameEx("lldr.inner")
### * lldr.inner

flush(stderr()); flush(stdout())

### Name: lldr.inner
### Title: lldr.inner
### Aliases: lldr.inner

### ** Examples

data(igm)
lcinc <- 10
Y <- igm$lcfincome
XMain <- igm$lfincome
XOther <- data.frame(COL=1*(igm$HEDUC=="COL"))
lldr.inner(lcinc, 10, Y, XMain, XOther)




cleanEx()
nameEx("llscm")
### * llscm

flush(stderr()); flush(stdout())

### Name: llscm
### Title: llscm
### Aliases: llscm

### ** Examples

data(igm)
igm$hs=ifelse(igm$HEDUC=="HS",1,0)
igm$col=ifelse(igm$HEDUC=="COL",1,0)
formla=lcfincome~lfincome
xformla=~hs+col
t=mean(igm$lfincome)
h=1.2
data=igm
llscm(formla,xformla,data,t,h)



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

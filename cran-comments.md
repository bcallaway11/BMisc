## Test environments

* local Ubuntu 20.04, R 4.2.1
* win-builder (devel and release)
* R-hub macOS 10.13.6 High Sierra, R-release, CRAN's setup

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTEs
  
## Downstream dependencies

There are four downstream dependencies: csabounds, did, DRDID, and qte.  These are all currently removed from CRAN due to BMisc being removed from CRAN. 

## Comments/Questions

The check problem was coming from the plm package (on which BMisc depends, but only through some examples) not being available on some platforms.  We have fixed this by making it only run these examples on platforms where plm is available.  I do have one question though: for the packages that depend on BMisc, will these need to be re-uploaded to CRAN or will this update to BMisc bring all of those back?

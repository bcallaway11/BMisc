In version 1.0.0, the BMisc package listed the qte package (which I am also the author and maintainer of) in the Imports field of the DESCRIPTION file.  It should not be there as the BMisc package does not depend on the qte package.  However, the qte package does depend on the BMisc package, and this caused a circular dependency.  This version of the BMisc package fixes that problem.

## Test environments

* local Linux Mint 18, R 3.2.3
* win-builder (devel and release)

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTES. 

## Downstream dependencies

There are no downstream dependencies.
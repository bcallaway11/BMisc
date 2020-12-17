## Test environments

* local Ubuntu 20.04, R 4.0.3
* win-builder (devel and release)
* R-hub (Windows Server, Ubuntu Linux, Fedora Linux)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTEs

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Brantly Callaway <brantly.callaway@uga.edu>'

  New maintainer:
    Brantly Callaway <brantly.callaway@uga.edu>
  Old maintainer(s):
    Brantly Callaway <bmcallaw@olemiss.edu.edu>

  I have changed email addresses and this updates to my new email.
  
## Downstream dependencies

We checked 4 reverse dependencies (3 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
 * There were new warnings generated in the `did` package (I am also the maintainer of this package) that were due to the function `panel2cs` in `BMisc` being deprecated.  To address this, I have (i) un-deprecated `panel2cs` in `BMisc`, (ii) will replace the call to `panel2cs` with the call to the new version of this function (`panel2cs2`) in the `did` package for its next version, and (iii) will then deprecate `panel2cs` again in the `BMisc` package for its next version.

## Test environments

* local Linux Mint 18, R 3.6.0
* win-builder (devel and release)
* rhub

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTES. 

## Downstream dependencies

Checked ccfa         : 0 errors | 1 warning  | 0 notes
Checked csabounds    : 0 errors | 0 warnings | 0 notes
Checked did          : 0 errors | 1 warning  | 0 notes
Checked qte          : 0 errors | 0 warnings | 0 notes
Checked TempleMetrics: 0 errors | 0 warnings | 0 notes

 * The warnings for both packages (which are also maintained by the maintainer of BMisc) are:

   Warning: replacing previous import ‘BMisc::lhs.vars’ by ‘formula.tools::lhs.vars’ when loading ‘ccfa’
  Warning: replacing previous import ‘BMisc::rhs.vars’ by ‘formula.tools::rhs.vars’ when loading ‘ccfa’
  Warning: replacing previous import ‘BMisc::rhs’ by ‘formula.tools::rhs’ when loading ‘ccfa’

  None of these functions are called in either package.  I am first updating 'BMisc' now, then I'll update 'did', and I'll fix these issues in 'ccfa' upon its next release.
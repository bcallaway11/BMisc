# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.4 (2018-03-15) |
|system   |x86_64, linux-gnu            |
|ui       |X11                          |
|language |en_US                        |
|collate  |en_US.UTF-8                  |
|tz       |America/New_York             |
|date     |2018-07-15                   |

## Packages

|package       |*  |version |date       |source                       |
|:-------------|:--|:-------|:----------|:----------------------------|
|BMisc         |*  |1.3.0   |2018-07-15 |local (bcallaway11/BMisc@NA) |
|formula.tools |   |1.7.1   |2018-03-01 |cran (@1.7.1)                |
|plm           |   |1.6-6   |2017-11-07 |cran (@1.6-6)                |

# Check results

2 packages with problems

|package |version | errors| warnings| notes|
|:-------|:-------|------:|--------:|-----:|
|ccfa    |1.0.0   |      0|        1|     0|
|did     |1.1.0   |      0|        1|     0|

## ccfa (1.0.0)
Maintainer: Weige Huang <weige.huang@temple.edu>

0 errors | 1 warning  | 0 notes

```
checking whether package ‘ccfa’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import ‘BMisc::lhs.vars’ by ‘formula.tools::lhs.vars’ when loading ‘ccfa’
  Warning: replacing previous import ‘BMisc::rhs.vars’ by ‘formula.tools::rhs.vars’ when loading ‘ccfa’
  Warning: replacing previous import ‘BMisc::rhs’ by ‘formula.tools::rhs’ when loading ‘ccfa’
See ‘/home/brant/Dropbox/BMisc/revdep/checks/ccfa.Rcheck/00install.out’ for details.
```

## did (1.1.0)
Maintainer: Brantly Callaway <brantly.callaway@temple.edu>

0 errors | 1 warning  | 0 notes

```
checking whether package ‘did’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import ‘BMisc::lhs.vars’ by ‘formula.tools::lhs.vars’ when loading ‘did’
  Warning: replacing previous import ‘BMisc::rhs.vars’ by ‘formula.tools::rhs.vars’ when loading ‘did’
  Warning: replacing previous import ‘BMisc::rhs’ by ‘formula.tools::rhs’ when loading ‘did’
See ‘/home/brant/Dropbox/BMisc/revdep/checks/did.Rcheck/00install.out’ for details.
```


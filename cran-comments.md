## Test environments

- Local Ubuntu 24.04, R 4.4.1:
    - All checks passed without issues.
- Github Actions
    - Windows-latest (R release)
    - Windows-latest (R devel)
    - macOS-latest (R release)
    - Ubuntu-latest (R release)
    - Ubuntu-latest (R devel)
    - All checks passed without issues.

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTEs

## Downstream dependencies

We checked 5 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

# Additional comments

* Removed 2nd URL that caused issue in last submission.
* Fixed bug related to `makeDist` function calling itself that caused issue in last submission.
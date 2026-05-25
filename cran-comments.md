## Test environments

- Local Ubuntu 24.04, R 4.6.0:
    - All checks passed without issues.
- Github Actions
    - Windows-latest (R release)
    - Windows-latest (R devel)
    - macOS-latest (R release)
    - Ubuntu-latest (R release)
    - Ubuntu-latest (R devel)
    - All checks passed without issues.

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE (local only)

The local Ubuntu 24.04 check produces one NOTE about a non-portable compilation
flag (`-mno-omit-leaf-frame-pointer`). This flag comes from Ubuntu's R 4.6.0
build configuration, not the package. It does not appear on any other platform.

## Downstream dependencies

We checked all 8 CRAN reverse dependencies using `revdeplite` (a
lightweight alternative to `revdepcheck`). We also checked the current
GitHub development versions of 8 downstream packages.

### CRAN versions

| Package    | Errors | Warnings | Notes | Status |
|------------|--------|----------|-------|--------|
| cdid       | 0      | 0        | 0     | PASS   |
| contdid    | 0      | 0        | 0     | PASS   |
| did        | 0      | 0        | 0     | PASS   |
| DRDID      | 0      | 1        | 0     | WARN   |
| fastdid    | 0      | 0        | 0     | PASS   |
| ptetools   | 0      | 1        | 0     | WARN   |
| qte        | 0      | 1        | 0     | WARN   |
| triplediff | 0      | 1        | 0     | WARN   |

The 4 warnings are all deprecation warnings from function names that were
renamed in this release (e.g. `rhs.vars` → `rhs_vars`). The deprecated
wrappers remain present in 1.4.9 so no package is broken — these are
warnings, not errors. We have already submitted fixes to the upstream
maintainers (see below).

### GitHub development versions

| Package    | Errors | Warnings | Notes | Status |
|------------|--------|----------|-------|--------|
| did        | 0      | 0        | 0     | PASS   |
| contdid    | 0      | 0        | 0     | PASS   |
| DRDID      | 0      | 0        | 0     | PASS   |
| cdid       | 0      | 0        | 0     | PASS   |
| fastdid    | 0      | 0        | 0     | PASS   |
| ptetools   | 0      | 3        | 0     | WARN   |
| qte        | 1      | 0        | 0     | FAIL   |
| triplediff | 0      | 1        | 0     | WARN   |

The GitHub versions of `did`, `contdid`, `DRDID`, `cdid`, and `fastdid`
all pass cleanly. The `ptetools` warnings are pre-existing documentation
and import issues unrelated to BMisc. The `qte` failure is a vignette
build error caused by an API mismatch between the development versions of
`qte` and `ptetools`, also unrelated to BMisc. The `triplediff` warning
is the BMisc deprecation; a fix has been submitted as
marcelortizv/triplediff#34.

## Additional comments

* This release adds snake_case aliases for 13 legacy function names
  (e.g. `makeBalancedPanel` → `make_balanced_panel`) and wraps the old
  names with `.Deprecated()`. The old names remain fully functional in
  this release; removal is planned for a future version once all
  downstream packages have updated.

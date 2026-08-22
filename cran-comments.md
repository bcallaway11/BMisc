## Test environments

- Local Ubuntu 24.04.4 LTS, R 4.6.1:
    - 0 errors, 0 warnings, 0 notes.
- GitHub Actions
    - Windows-latest (R release)
    - Windows-latest (R devel)
    - macOS-latest (R release)
    - Ubuntu-latest (R release)
    - Ubuntu-latest (R devel)
    - All checks passed without issues.

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTEs

## Downstream dependencies

We checked all 8 CRAN reverse dependencies using `revdeplite` (a
lightweight alternative to `revdepcheck`). We also checked the current
GitHub development versions of 4 downstream packages.

### CRAN versions

| Package    | Errors | Warnings | Notes | Status |
|------------|--------|----------|-------|--------|
| cdid       | 0      | 0        | 0     | PASS   |
| contdid    | 0      | 0        | 0     | PASS   |
| did        | 0      | 0        | 0     | PASS   |
| DRDID      | 0      | 0        | 0     | PASS   |
| fastdid    | 0      | 0        | 0     | PASS   |
| ptetools   | 0      | 0        | 0     | PASS   |
| qte        | 0      | 0        | 0     | PASS   |
| triplediff | 0      | 0        | 0     | PASS   |

All 8 CRAN reverse dependencies pass cleanly with no errors, warnings,
or notes.

### GitHub development versions

| Package    | Errors | Warnings | Notes | Status |
|------------|--------|----------|-------|--------|
| DRDID      | 0      | 0        | 0     | PASS   |
| ptetools   | 0      | 0        | 0     | PASS   |
| did        | 0      | 2        | 0     | WARN   |
| csabounds  | 0      | 0        | 1     | NOTE   |

The `did` warnings are a pre-existing vignette-build housekeeping issue
(missing `inst/doc`), unrelated to BMisc and not present in its CRAN
release. The `csabounds` note is a pre-existing Rd markup-escaping issue,
also unrelated to BMisc.

## Additional comments

* Fixes a bug where several panel getters (`get_group()` and others)
  silently misassigned output rows for panels not sorted by id.
* `make_balanced_panel()`'s `return_data.table` argument was removed
  (unused by all reverse dependencies); it now returns the same class
  it was given.

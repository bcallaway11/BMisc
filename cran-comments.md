## Submission summary

This is a minor release (1.4.11). Main changes:

* Bug fixes:
    * `quantile()` was wrong for distribution functions created by
      `make_dist()`, `weighted_ecdf()` and `combine_ecdfs()` when there were
      ties or unequal weights. These objects now have class `BMisc_dist`
      (they are still `ecdf` objects) with an exact `quantile()` method.
    * `invert_ecdf()` now returns the smallest point where the distribution
      function reaches the probability.
    * `get_group()` gave the wrong group when a unit's rows were not sorted
      by time.
    * `orig2t()` and `t2orig()` returned a list for values that are not
      time periods; they now return `NA`.
* `combine_ecdfs()` now computes the mixture exactly by default.
* Faster `multiplier_bootstrap()`, `block_boot_sample()`, `orig2t()`,
  `t2orig()` and `weighted_ecdf()`. For `multiplier_bootstrap()` the
  Rademacher weights are drawn differently, so for a given seed its
  bootstrap draws (and bootstrap standard errors in downstream packages)
  differ slightly from earlier versions. This is noted in NEWS.md.
* Documentation: return values added for the panel getters; Markdown is
  now enabled in the Rd files.

## Test environments

- Local Ubuntu 26.04.1 LTS, R 4.5.2
- GitHub Actions (`R CMD check --no-manual`), all Status: OK (0 errors,
  0 warnings, 0 notes):
    - Windows-latest, R 4.6.2 (release)
    - Windows-latest, R-devel (2026-11-17 r90926)
    - macOS-latest, R 4.6.2 (release)
    - Ubuntu-latest, R 4.6.2 (release)
    - Ubuntu-latest, R-devel (2026-11-19 r90939)

## R CMD check results

`R CMD check --as-cran`: 0 errors | 0 warnings | 0 notes, apart from notes
that come from the local setup: the current time could not be verified,
HTML Tidy and V8 are not installed, a compiler flag set by the system's R
build (`-mno-omit-leaf-frame-pointer`), and a transient HTTP 429 (rate
limit) from GitHub when checking a README URL.

## Downstream dependencies

We checked all 7 CRAN reverse dependencies with `revdeplite` (a
lightweight alternative to `revdepcheck`). All pass with no errors,
warnings or notes.

| Package    | Errors | Warnings | Notes | Status |
|------------|--------|----------|-------|--------|
| contdid    | 0      | 0        | 0     | PASS   |
| did        | 0      | 0        | 0     | PASS   |
| DRDID      | 0      | 0        | 0     | PASS   |
| fastdid    | 0      | 0        | 0     | PASS   |
| ptetools   | 0      | 0        | 0     | PASS   |
| qte        | 0      | 0        | 0     | PASS   |
| triplediff | 0      | 0        | 0     | PASS   |

We also checked the GitHub development versions of DRDID, ptetools and
contdid (all pass).

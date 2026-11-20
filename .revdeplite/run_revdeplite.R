# =============================================================================
# Title: Run revdeplite
# Description: Run lightweight reverse dependency checks for BMisc: all CRAN
#   reverse dependencies, plus the GitHub development versions of packages
#   that will be released against this version of BMisc.
# Author: Brant Callaway
# Last update: 2026-11-20
# Date created: 2026-11-20
# =============================================================================

revdeplite::revdeplite(
  github_deps = c(
    "bcallaway11/did",
    "bcallaway11/csabounds",
    "pedrohcgs/DRDID",
    "bcallaway11/ptetools",
    "bcallaway11/contdid",
    "bcallaway11/qte"
  ),
  check_dir = ".revdeplite",
  num_cores = 4
)

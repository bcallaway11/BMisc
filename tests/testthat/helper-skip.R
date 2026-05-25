# Run slow tests: R_SLOW_TESTS=true Rscript -e "devtools::test()"
skip_slow <- function() {
  if (!identical(Sys.getenv("R_SLOW_TESTS"), "true")) {
    testthat::skip("slow test — set R_SLOW_TESTS=true to run")
  }
}

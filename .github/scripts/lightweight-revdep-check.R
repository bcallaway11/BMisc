# =============================================================================
# Title: Lightweight Reverse Dependency Check
# Description: Check CRAN tarballs and selected GitHub repositories against the
#   locally installed BMisc package in GitHub Actions.
# Author: Brant Callaway
# Last update: 2026-05-25
# Date created: 2026-05-25
# =============================================================================

options(repos = c(CRAN = "https://cloud.r-project.org"))

# --- Helpers -------------------------------------------------------------

split_csv <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    return(character())
  }
  out <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  out[nzchar(out)]
}

clean_name <- function(x) {
  gsub("[^A-Za-z0-9_.-]+", "-", x)
}

write_lines <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(x, path)
}

append_result <- function(results, source, package, location, status, details) {
  rbind(
    results,
    data.frame(
      source = source,
      package = package,
      location = location,
      status = status,
      details = details,
      stringsAsFactors = FALSE
    )
  )
}

check_status <- function(check) {
  if (length(check$errors) > 0L) {
    return("error")
  }
  if (length(check$warnings) > 0L) {
    return("warning")
  }
  if (length(check$notes) > 0L) {
    return("note")
  }
  "ok"
}

check_details <- function(check) {
  paste0(
    "errors=", length(check$errors),
    "; warnings=", length(check$warnings),
    "; notes=", length(check$notes)
  )
}

install_revdep_deps <- function(path) {
  remotes::install_deps(path, dependencies = TRUE, upgrade = "never")
}

run_check <- function(path, check_dir) {
  dir.create(check_dir, recursive = TRUE, showWarnings = FALSE)
  rcmdcheck::rcmdcheck(
    path,
    args = "--no-manual",
    error_on = "never",
    check_dir = check_dir
  )
}

# --- Setup ---------------------------------------------------------------

mode <- Sys.getenv("REVDEP_SOURCE", unset = "both")
github_repos <- split_csv(Sys.getenv("REVDEP_GITHUB_REPOS", unset = ""))
cran_subset <- split_csv(Sys.getenv("REVDEP_CRAN_PACKAGES", unset = ""))

result_dir <- "revdep-results"
source_dir <- file.path(result_dir, "sources")
tarball_dir <- file.path(result_dir, "tarballs")
check_dir <- file.path(result_dir, "checks")

dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tarball_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(check_dir, recursive = TRUE, showWarnings = FALSE)

results <- data.frame(
  source = character(),
  package = character(),
  location = character(),
  status = character(),
  details = character(),
  stringsAsFactors = FALSE
)

cat("Checking reverse dependencies against BMisc version",
    as.character(utils::packageVersion("BMisc")), "\n")

# --- CRAN Reverse Dependencies ------------------------------------------

if (mode %in% c("cran", "both")) {
  available <- available.packages()
  cran_revdeps <- tools::package_dependencies(
    packages = "BMisc",
    db = available,
    reverse = TRUE
  )[["BMisc"]]

  if (length(cran_subset) > 0L) {
    cran_revdeps <- intersect(cran_revdeps, cran_subset)
  }

  cat("CRAN reverse dependencies:", paste(cran_revdeps, collapse = ", "), "\n")

  for (pkg in cran_revdeps) {
    cat("Checking CRAN package:", pkg, "\n")
    pkg_check_dir <- file.path(check_dir, paste0("cran-", clean_name(pkg)))

    result <- tryCatch({
      tarball_info <- download.packages(
        pkg,
        destdir = tarball_dir,
        type = "source"
      )
      tarball <- tarball_info[1L, 2L]

      extract_dir <- file.path(source_dir, paste0("cran-", clean_name(pkg)))
      dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
      untar(tarball, exdir = extract_dir)
      extracted_path <- list.dirs(
        extract_dir,
        recursive = FALSE,
        full.names = TRUE
      )[1L]

      install_revdep_deps(extracted_path)
      check <- run_check(tarball, pkg_check_dir)

      list(status = check_status(check), details = check_details(check))
    }, error = function(e) {
      list(status = "setup-error", details = conditionMessage(e))
    })

    results <- append_result(
      results,
      source = "cran",
      package = pkg,
      location = pkg,
      status = result$status,
      details = result$details
    )
  }
}

# --- GitHub Reverse Dependencies ----------------------------------------

if (mode %in% c("github", "both")) {
  cat("GitHub repositories:", paste(github_repos, collapse = ", "), "\n")

  for (repo in github_repos) {
    pkg <- basename(repo)
    cat("Checking GitHub repository:", repo, "\n")

    repo_dir <- file.path(source_dir, paste0("github-", clean_name(repo)))
    pkg_check_dir <- file.path(check_dir, paste0("github-", clean_name(repo)))
    repo_url <- paste0("https://github.com/", repo, ".git")

    result <- tryCatch({
      clone_status <- system2(
        "git",
        args = c("clone", "--depth", "1", repo_url, repo_dir)
      )
      if (!identical(clone_status, 0L)) {
        stop("git clone failed with status ", clone_status)
      }

      install_revdep_deps(repo_dir)
      check <- run_check(repo_dir, pkg_check_dir)

      list(status = check_status(check), details = check_details(check))
    }, error = function(e) {
      list(status = "setup-error", details = conditionMessage(e))
    })

    results <- append_result(
      results,
      source = "github",
      package = pkg,
      location = repo,
      status = result$status,
      details = result$details
    )
  }
}

# --- Summary -------------------------------------------------------------

summary_csv <- file.path(result_dir, "summary.csv")
summary_md <- file.path(result_dir, "summary.md")

utils::write.csv(results, summary_csv, row.names = FALSE)

if (nrow(results) == 0L) {
  stop("No reverse dependencies were selected for checking")
}

result_lines <- apply(
  results,
  1L,
  function(row) {
    paste0(
      "| ", row[["source"]],
      " | ", row[["package"]],
      " | ", row[["location"]],
      " | ", row[["status"]],
      " | ", row[["details"]],
      " |"
    )
  }
)

summary_lines <- c(
  "# Lightweight reverse dependency check",
  "",
  paste0("- BMisc version: ", as.character(utils::packageVersion("BMisc"))),
  paste0("- Source mode: ", mode),
  "",
  "| source | package | location | status | details |",
  "|:--|:--|:--|:--|:--|",
  result_lines
)

write_lines(summary_lines, summary_md)
print(results)

bad_status <- results$status %in% c("error", "warning", "setup-error")
if (any(bad_status)) {
  stop("Reverse dependency checks found errors, warnings, or setup errors")
}

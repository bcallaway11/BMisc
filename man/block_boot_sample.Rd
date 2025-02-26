% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BMisc.R
\name{block_boot_sample}
\alias{block_boot_sample}
\title{Block Bootstrap}
\usage{
block_boot_sample(data, idname)
}
\arguments{
\item{data}{data.frame from which you want to bootstrap}

\item{idname}{column in data which contains an individual identifier}
}
\value{
data.frame bootstrapped from the original dataset; this data.frame
 will contain new ids
}
\description{
make draws of all observations with the same id in a panel
 data context.  This is useful for bootstrapping with panel data.
}
\examples{
\dontshow{
if (!requireNamespace("plm")) {
  if (interactive() || is.na(Sys.getenv("_R_CHECK_PACKAGE_NAME_", NA))) {
    stop("package 'plm' is required for this example")
  } else {
    q()
  }
}
}
data("LaborSupply", package = "plm")
bbs <- block_boot_sample(LaborSupply, "id")
nrow(bbs)
head(bbs$id)

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BMisc.R
\name{get_Yibar_inner}
\alias{get_Yibar_inner}
\title{get_Yibar_inner}
\usage{
get_Yibar_inner(this_df, yname)
}
\arguments{
\item{this_df}{a data.frame, for this function it should be specific to
a particular unit}

\item{yname}{name of column containing the outcome (or other variable)
for which to calculate its outcome in the immediate pre-treatment period}
}
\description{
Calculates a units average outcome across all periods.
 This function operates on a data.frame that is already local to a particular
 unit.
}
\keyword{internal}

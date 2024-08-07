% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BMisc.R
\name{orig2t}
\alias{orig2t}
\title{orig2t}
\usage{
orig2t(orig, original_time.periods)
}
\arguments{
\item{orig}{a vector of original time periods to convert to new time periods.}

\item{original_time.periods}{vector containing all original time periods.}
}
\value{
new time period converted from original time period
}
\description{
A helper function to switch from original time periods to
 "new" time periods (which are just time periods going from 1 to total
 number of available periods).  This allows for periods not being
 exactly spaced apart by 1.
}

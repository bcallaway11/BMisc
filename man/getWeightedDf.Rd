% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BMisc.R
\name{getWeightedDf}
\alias{getWeightedDf}
\title{Weighted Distribution Function}
\usage{
getWeightedDf(y, y.seq = NULL, weights = NULL, norm = TRUE)
}
\arguments{
\item{y}{a vector to compute the mean for}

\item{y.seq}{an optional vector of values to compute the distribution function
for; the default is to use all unique values of y}

\item{weights}{the vector of weights, can be NULL, then will just return mean}

\item{norm}{normalize the weights so that they have mean of 1, default is
to normalize}
}
\value{
ecdf
}
\description{
Get a distribution function from a vector of values
 after applying some weights
}

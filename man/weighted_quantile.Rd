% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BMisc.R
\name{weighted_quantile}
\alias{weighted_quantile}
\title{weighted_quantile}
\usage{
weighted_quantile(tau, cvec, weights = NULL, norm = TRUE)
}
\arguments{
\item{tau}{a vector of values between 0 and 1}

\item{cvec}{a vector to compute quantiles for}

\item{weights}{the weights, weighted.checkfun normalizes the weights
to sum to 1.}

\item{norm}{normalize the weights so that they have mean of 1, default is
to normalize}
}
\value{
vector of quantiles
}
\description{
function to recover quantiles of a vector with weights
}

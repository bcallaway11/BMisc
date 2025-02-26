% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BMisc.R
\name{weighted_combine_list}
\alias{weighted_combine_list}
\title{weighted_combine_list}
\usage{
weighted_combine_list(l, w, normalize_weights = TRUE)
}
\arguments{
\item{l}{a list that contains either vectors or matrices of the same dimension
that are to be combined}

\item{w}{a vector of weights, the weights should have the same number
of elements as `length(l)`}

\item{normalize_weights}{whether or not to force the weights to sum to 1,
default is true}
}
\value{
matrix or vector corresponding to the weighted average of
 all of the elements in `l`
}
\description{
A function that takes in either a list of vectors or matrices
 and computes a weighted average of them, where the weights are applied to
 every element in the list.
}

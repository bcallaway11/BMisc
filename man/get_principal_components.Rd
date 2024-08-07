% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BMisc.R
\name{get_principal_components}
\alias{get_principal_components}
\title{get_principal_components}
\usage{
get_principal_components(xformula, data, idname, tname, n_components = NULL)
}
\arguments{
\item{xformula}{a formula specifying the variables to use in the principal component analysis}

\item{data}{a data.frame containing the panel data}

\item{idname}{the name of the column containing the unit id}

\item{tname}{the name of the column containing the time period}

\item{n_components}{the number of principal components to retain, the default is NULL which 
will result in all principal components being retained}
}
\value{
a data.frame containing the original data with the principal components appended
}
\description{
A function to calculate unit-specific principal components, given panel data
}

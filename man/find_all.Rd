% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algorithm.R
\name{find_all}
\alias{find_all}
\title{Return list of index of from which is matched to the argument}
\usage{
find_all(what, from, condition = `==`)
}
\arguments{
\item{what}{value of sequence for finding}

\item{from}{index of this argument is returned}

\item{condition}{condition for finding. Default is "=="}
}
\value{
List of index which is found in from at each what
}
\description{
Return list of index of from which is matched to the argument
}
\examples{
find_all(c(1,3,5), c(0,1,2,3,3))
# list(2,c(4,5),integer(0))
}

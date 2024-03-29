% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{source_r}
\alias{source_r}
\title{read R code from a file, with changing directory.}
\usage{
source_r(file, local = TRUE, ..., chdir = TRUE)
}
\arguments{
\item{file}{file name}

\item{local}{local environment; default is current environment}

\item{...}{argument for source}

\item{chdir}{change dir}
}
\description{
Same with default source except: 1. local is TRUE in default, 2. chdir is TRUE in default.
}

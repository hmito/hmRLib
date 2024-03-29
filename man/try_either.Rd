% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exception.R
\name{try_either}
\alias{try_either}
\title{Try and catch_either.}
\usage{
try_either(expr, alt_value, catchfn = NULL, silent = TRUE)
}
\arguments{
\item{expr}{Function which should be executed.}

\item{alt_value}{return value when expr throw exception.}

\item{catchfn}{Function which will be called when expr throw exception with error message as argument.}

\item{silent}{Logical value: if TRUE, the error message is not shown.}
}
\description{
Try and catch_either.
}
\examples{
f = function(i){
   if(i<0)stop("negative value")
   return(i)
}
x = try_either(f(3),NULL)
#x == 3
a = try_either(f(-1),NULL)
#a == NULL
b = try_either(f(-1),0)
#b == 0
}

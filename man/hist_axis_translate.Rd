% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/histgram.R
\name{hist_axis_translate}
\alias{hist_axis_translate}
\title{Translate axis of histgram sequence into another axis}
\usage{
hist_axis_translate(src.hist, src.breakseq, trg.breakseq)
}
\arguments{
\item{src.hist}{Source histgram sequence of float values.}

\item{src.breakseq}{Source histgram breaks. The length is length(src.hist)+1.}

\item{trg.breakseq}{Target histgram breaks.}
}
\value{
Target histgram sequence of float values.
}
\description{
This function translate the histgram sequence with a certain axis into another histgram sequence with a different axis.
}
\examples{
x = c(1,2,3)
x.ax = c(0,1,2,3)
y.ax = c(0,0.5,1,1.5,2,2.5)
hist_axis_translate(x,x.ax,y.ax)
# c(0.5,0.5,1,1,1.5)
}

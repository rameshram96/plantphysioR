% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plantphysioR.R
\name{st_index}
\alias{st_index}
\title{Calculate Stress tolerance index (STI) suggested by Fernandez (1992)}
\usage{
st_index(Yp, Ys)
}
\arguments{
\item{Yp}{Yield under control condition}

\item{Ys}{Yield under stress condition}
}
\value{
STI
}
\description{
The genotype with high STI values will be tolerant to drought
}
\examples{
st_index(500, 350)
}
\references{
Fernandez, G. C. (1993). Effective selection criteria for assessing plant stress tolerance.
}

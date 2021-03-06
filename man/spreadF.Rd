% Generated by roxygen2 (4.0.0): do not edit by hand
\name{spreadF}
\alias{spreadF}
\title{spreadF}
\usage{
spreadF(fpSI, fpIR, stepsize = 1)
}
\arguments{
\item{fpSI}{a function of t and vertices that returns the infection probability}

\item{fpIR}{a function of t and vertices that returns the recovery probability}

\item{stepsize}{time step}
}
\value{
a spreader function
}
\description{
functional probability epi modelling
}
\details{
spread an SIR model using a function of time and attributes
}
\author{
Barry S Rowlingson
}


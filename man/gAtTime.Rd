\name{gAtTime}
\alias{gAtTime}
\title{gAtTime}
\usage{
gAtTime(g, t)
}
\arguments{
  \item{g}{an SIR graph}

  \item{t}{a time point}
}
\value{
the SIR graph at that time
}
\description{
recalculate state of network at given time
}
\details{
when a simulation has run, the graph stores the infection
and recovery times, and the final states. This function
sets the states to the values correct at the given time by
comparison with the infection and recovery times. It also
sets the time of the graph but does not change the
infection and recovery times.
}
\author{
Barry S Rowlingson
}


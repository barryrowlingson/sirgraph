\name{stepSim}
\alias{stepSim}
\title{stepSim}
\usage{
  stepSim(g, starter = infectN(1), spreader, stopper,
    after = force)
}
\arguments{
  \item{g}{an SIR graph}

  \item{starter}{a starter function}

  \item{spreader}{a spreader function}

  \item{stopper}{a stopper function}

  \item{after}{function to call after each spread function
  call}
}
\value{
  the end state of the graph
}
\description{
  Discrete time-stepping SIR graph model
}
\details{
  Starting with a graph, infect the initial cases, then run
  the spreader until the stopper is true. Optionally run
  the plotter every iteration
}
\author{
  Barry S Rowlingson
}


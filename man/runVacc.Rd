\name{runVacc}
\alias{runVacc}
\title{runVacc}
\usage{
runVacc(ni, pVaccine = c(0, 0.9), pIR = 0.3, g)
}
\arguments{
  \item{ni}{number of initial cases}

  \item{pVaccine}{vector of unvaccinated/vaccinated
  transmission probability}

  \item{pIR}{single probability of recovert}

  \item{g}{graph structure}
}
\value{
SIR graph after infection has finished
}
\description{
example SIR with vaccination
}
\details{
run an SIR model with vaccination
}
\author{
Barry S Rowlingson
}


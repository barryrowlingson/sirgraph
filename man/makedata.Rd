\name{makedata}
\alias{makedata}
\title{makedata}
\usage{
  makedata(g = graph.famous("Zachary"), s = 310366,
    pvac = 0.6, start = as.Date("2014-01-01"),
    stepsize = 1)
}
\arguments{
  \item{g}{a simple graph}

  \item{s}{a random seed}

  \item{pvac}{vaccination probability}

  \item{start}{time start}

  \item{stepsize}{size of time step}
}
\value{
  a modified version of g with extra vertex attributes
}
\description{
  make a simple graph for SIR modelling
}
\details{
  make a graph for SIR modelling - it initialises the
  network and the state parameter, and adds a few more
  random things such as age, sex, vaccination state. Handy
  for simple tests
}
\author{
  Barry S Rowlingson
}


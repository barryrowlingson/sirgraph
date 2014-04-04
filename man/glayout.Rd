\name{glayout}
\alias{glayout}
\title{glayout}
\usage{
  glayout(g, layout = layout.kamada.kawai, ...)
}
\arguments{
  \item{g}{an SIR graph}

  \item{layout}{a layout function from igraph}

  \item{...}{further parameters for the layout function}
}
\value{
  a modified SIR graph
}
\description{
  add layout to graph
}
\details{
  this function adds x and y coordinate to nodes for
  plotting
}
\author{
  Barry S Rowlingson
}


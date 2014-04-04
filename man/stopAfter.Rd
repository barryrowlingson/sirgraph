\name{stopAfter}
\alias{stopAfter}
\title{stopAfter}
\usage{
  stopAfter(d)
}
\arguments{
  \item{d}{anything that can be compared to the time
  attribute of the graph}
}
\value{
  a stopper function that returns whether or not the time
  is past.
}
\description{
  Stop after a certain time
}
\details{
  This is a stopper function generator returns a function
  that stops a process after a given time.
}
\author{
  Barry S Rowlingson
}


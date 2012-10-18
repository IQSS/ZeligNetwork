\name{param.normal.net}
\alias{param.normal.net}
\title{Extract Samples from a Distribution in Order to Pass Them to the \code{qi}
Function
(this is primarily a helper function for the normal.net model)}
\usage{
  param.normal.net(obj, num = 1000, ...)
}
\arguments{
  \item{obj}{a zelig object}

  \item{num}{an integer specifying the number of
  simulations to compute}

  \item{...}{additional parameters}
}
\value{
  a list specifying link, link-inverse, random samples, and
  ancillary parameters
}
\description{
  Extract Samples from a Distribution in Order to Pass Them
  to the \code{qi} Function (this is primarily a helper
  function for the normal.net model)
}


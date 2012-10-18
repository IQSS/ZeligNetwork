\name{param.ls.net}
\alias{param.ls.net}
\title{Extract Samples from a Distribution in Order to Pass Them to the \code{qi}
Function
(this is primarily a helper function for the ls.net model)}
\usage{
  param.ls.net(obj, num = 1000, ...)
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
  function for the ls.net model)
}


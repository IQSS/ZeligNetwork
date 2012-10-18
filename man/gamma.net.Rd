\name{gamma.net}
\alias{gamma.net}
\title{Fitting Gamma Network Models}
\usage{
  gamma.net(formula, data, LF = "inverse",
    family = Gamma(link = LF), ..., mode = "digraph",
    diag = FALSE, nullhyp = NULL, tol = 1e-07, reps = 1000)
}
\arguments{
  \item{formula}{formula}

  \item{data}{data.frame}

  \item{LF}{character-string}

  \item{family}{family object}

  \item{...}{additional parameters}

  \item{mode}{character-string}

  \item{diag}{logical}

  \item{nullhyp}{character-vector or NULL}

  \item{tol}{numeric}

  \item{reps}{integer}
}
\value{
  Some kind of model
}
\description{
  Fit social network data.
}


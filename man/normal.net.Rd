\name{normal.net}
\alias{normal.net}
\title{Fitting Gaussian Network Models}
\usage{
  normal.net(formula, data, LF = "identity",
    family = gaussian(link = LF), ..., mode = "digraph",
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


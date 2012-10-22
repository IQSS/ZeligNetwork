\name{netgamma}
\alias{netgamma}
\title{Fitting C-log-log Network Models}
\usage{
  netgamma(formula, data, LF = "inverse",
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
  an object of ``netglm'' class
}
\description{
  Fit social network data.
}


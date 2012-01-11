\name{poisson.net}
\alias{poisson.net}
\title{Fitting Poisson Network Models...}
\usage{poisson.net(formula, data, LF="log", family=poisson(link = LF), ...,
    mode="digraph", diag=FALSE, nullhyp, tol=1e-07, reps=1000)}
\description{Fitting Poisson Network Models}
\details{Fit social network data.}
\value{Some kind of model}
\arguments{\item{formula}{formula}
\item{data}{data.frame}
\item{LF}{character-string}
\item{family}{family object}
\item{...}{additional parameters}
\item{mode}{character-string}
\item{diag}{logical}
\item{nullhyp}{character-vector or NULL}
\item{tol}{numeric}
\item{reps}{integer}}

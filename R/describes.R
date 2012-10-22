#' Describe a `logit' model to Zelig
#' @usage
#' \method{describe}{cloglog.net}(...)
#' \method{describe}{gamma.net}(...)
#' \method{describe}{logit.net}(...)
#' \method{describe}{ls.net}(...)
#' \method{describe}{normal.net}(...)
#' \method{describe}{poisson.net}(...)
#' \method{describe}{probit.net}(...)
#' @S3method describe cloglog.net
#' @S3method describe gamma.net
#' @S3method describe logit.net
#' @S3method describe ls.net
#' @S3method describe normal.net
#' @S3method describe poisson.net
#' @S3method describe probit.net
#' @aliases
#' describe.cloglog.net
#' describe.gamma.net
#' describe.logit.net
#' describe.ls.net
#' describe.normal.net
#' describe.poisson.net
#' describe.probit.net
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @name describes
describe.cloglog.net <- function(...) {
  list(
       authors = "Skyler J. Cranmer",
       text = "Social Network Complementary Log Log Regression for Dichotomous Dependent Variables",
       year = 2012
       )
}

describe.gamma.net <- function(...) {
  list(
       authors = "Skyler J. Cranmer",
       text = "Social Network Gamma Regression for Continuous, Positive Dependent Variables",
       year = 2012
       )
}

describe.logit.net <- function(...) {
  list(
       authors = "Skyler J. Cranmer",
       text = "Social Network Logistic Regression for Dichotomous Dependent Variables",
       year = 2012
       )
}

describe.ls.net <- function(...) {
  list(
       authors = "Skyler J. Cranmer",
       text = "Social Network Least Squares Regression for Continuous Dependent Variables",
       year = 2012
       )
}

describe.normal.net <- function(...) {
  list(
       authors = "Skyler J. Cranmer",
       text = "Social Network Gaussian Regression for Continuous Dependent Variables",
       year = 2012
       )
}

describe.poisson.net <- function(...) {
  list(
       authors = "Skyler J. Cranmer",
       text = "Social Network Poisson Regression for Event Count Dependent Variables",
       year = 2012
       )
}

describe.probit.net <- function(...) {
  list(
       authors = "Skyler J. Cranmer",
       text = "Social Network Probit Regression for Dichotomous Dependent Variables",
       year = 2012
       )
}

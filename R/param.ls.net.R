#' @S3method param ls.net
param.ls.net <- function(obj, num=1000, ...) {
  list(coef = mvrnorm(num, coef(obj), vcov(obj)))
}

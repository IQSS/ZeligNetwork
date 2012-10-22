#' @S3method param cloglog.net
param.cloglog.net <- function(obj, num=1000, ...) {
  list(
       coef = mvrnorm(num, coef(obj), vcov(obj)),
       fam = binomial(link="cloglog")
       )
}

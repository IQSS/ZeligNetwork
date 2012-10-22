#' @S3method param logit.net
param.logit.net <- function(obj, num=1000, ...) {
  list(
       coef = mvrnorm(num, coef(obj), vcov(obj)),
       fam = binomial(link="logit")
       )
}

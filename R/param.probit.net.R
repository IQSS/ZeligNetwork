#' @S3method param probit.net
param.probit.net <- function(obj, num=1000, ...) {
  list(
       coef = mvrnorm(num, coef(obj), vcov(obj)),
       fam = binomial(link="logit")
       )
}

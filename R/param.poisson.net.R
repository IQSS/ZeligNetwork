#' @S3method param poisson.net
param.poisson.net <- function(obj, num=1000, ...) {
  #print(obj$result$family)
  list(
       coef = mvrnorm(num, coef(obj), vcov(obj)),
       fam = poisson()
       )
}

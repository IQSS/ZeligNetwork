#' @S3method param gamma.net
param.gamma.net <- function(obj, num=1000, ...) {

  gamma.shape <- getS3method('gamma.shape', 'glm')
  rate <- gamma.shape(obj$result) 

  list(
       coef  = mvrnorm(num, coef(obj), vcov(obj)),
       alpha = rnorm(num, rate$alpha, rate$SE),
       fam   = Gamma()
       )
}

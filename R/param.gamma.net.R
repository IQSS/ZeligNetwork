#' Extract Samples from a Distribution in Order to Pass Them to the \code{qi} 
#' Function
#' (this is primarily a helper function for the gamma.net model)
#' @param obj a zelig object
#' @param num an integer specifying the number of simulations to compute
#' @param ... additional parameters
#' @return a list specifying link, link-inverse, random samples, and ancillary
#' parameters
#' @export
param.gamma.net <- function(obj, num=1000, ...) {

  gamma.shape <- getS3method('gamma.shape', 'glm')
  rate <- gamma.shape(obj$result) 

  list(
       coef  = mvrnorm(num, coef(obj), vcov(obj)),
       alpha = rnorm(num, rate$alpha, rate$SE),
       fam   = Gamma()
       )
}

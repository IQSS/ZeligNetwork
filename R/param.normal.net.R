#' Extract Samples from a Distribution in Order to Pass Them to the \code{qi} 
#' Function
#' (this is primarily a helper function for the normal.net model)
#' @param obj a zelig object
#' @param num an integer specifying the number of simulations to compute
#' @param ... additional parameters
#' @return a list specifying link, link-inverse, random samples, and ancillary 
#' parameters
#' @export
param.normal.net <- function(obj, num=1000, ...) {
  df <- obj$result$df.residual
  sig2 <- summary(obj)$dispersion

  list(
       coef = mvrnorm(num, coef(obj), vcov(obj)),
       alpha = sqrt(df*sig2/rchisq(num, df=df)),
       fam = gaussian()
       )
}

#' @S3method param normal.net
param.normal.net <- function(obj, num=1000, ...) {
  df <- obj$result$df.residual
  sig2 <- summary(obj)$dispersion

  list(
       coef = mvrnorm(num, coef(obj), vcov(obj)),
       alpha = sqrt(df*sig2/rchisq(num, df=df)),
       fam = gaussian()
       )
}

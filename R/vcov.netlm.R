#' @S3method vcov netglm
vcov.netlm <- function(object, ...) {
  so <- summary.glm(object, correlation = FALSE, ...)
  VCOV <- so$dispersion * so$cov.unscaled


  # mf <- model.frame(formula(object), object$data)
  mm <- model.matrix(formula(object), object$data)

  # Correctly name columns and rows
  colnames(VCOV) <- rownames(VCOV) <- colnames(mm)

  # Return covariance matrix
  VCOV
}

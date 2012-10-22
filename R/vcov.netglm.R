#' @S3method vcov netglm
vcov.netglm <- function(object, ...) {
  so <- summary.glm(object, correlation = FALSE, ...)
  VCOV <- so$dispersion * so$cov.unscaled


  # mf <- model.frame(formula(object), object$data)
  mm <- model.matrix(formula(object), object$data)

  # Match correct names
  colnames(VCOV) <- rownames(VCOV) <- colnames(mm)

  # Return
  VCOV
}

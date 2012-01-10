#' Calculate Variance-Covariance Matrix for a `netglm' Fitted Model Object
#'
#' Returns the variance-covariance matrix for all `netglm' fitted model objects.
#' @usage \method{vcov}{netglm}(object, ...)
#' @S3method vcov netglm
#' @param obj a `netglm' object
#' @param ... parameters to be passed to `summary.glm'
#' @return a matrix
vcov.netglm <- function(object, ...)
{
    so <- summary.glm(object, corr=FALSE, ...)
    VCOV <- so$dispersion * so$cov.unscaled


    # mf <- model.frame(formula(object), object$data)
    mm <- model.matrix(formula(object), object$data)


    colnames(VCOV) <- rownames(VCOV) <- colnames(mm)
    VCOV
}

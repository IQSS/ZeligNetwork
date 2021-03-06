#' @S3method qi ls.net
qi.ls.net <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  beta <- coef(param)

  # Compute Expected Values
  #
  # ...
  # @param simulations ...
  # @param alpha ...
  # @param x
  # @return ...
  compute.ev <- function (simulations, x) {

    # Ensure 'setx' object is valid
    if (is.null(x) || is.na(x))
      return(NA)

    # Construct eta, the value of the linear predictors before the 
    # inverse-link function is applied
    eta <- beta %*% t(x)

    # Construct theta, the f^-1(eta)
    theta <- matrix(eta, nrow=nrow(beta))

    # Properly name the matrix dimensions
    dimnames(theta) <- dimnames(eta)

    # Return
    theta
  }

  ev1 <- compute.ev(beta, x)
  ev2 <- compute.ev(beta, x1)

  list(
       "Expected Value (for X): E(Y|X)" = ev1,
       "Expected Value (for X1): E(Y|X1)" = ev2,
       "First Differences: E(Y|X1)-E(Y|X)" = ev2 - ev1
       )
}

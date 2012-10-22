#' @S3method qi logit.net
qi.logit.net <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  inv <- linkinv(param)
  beta <- coef(param)

  # compute expected values
  #
  # ...
  # @param simulations ...
  # @param alpha ...
  # @param x
  # @return ...
  compute.ev <- function (simulations, x) {
    # ensure 'setx' object is valid
    if (is.null(x) || is.na(x))
      return(NA)

    # construct eta, the value of the linear predictors before the 
    # inverse-link function is applied
    eta <- beta %*% t(x)

    # construct theta, the f^-1(eta)
    theta <- matrix(inv(eta), nrow=nrow(beta))

    # properly name the matrix dimensions
    dimnames(theta) <- dimnames(eta)

    # return
    theta
  }

  ev1 <- compute.ev(beta, x)
  ev2 <- compute.ev(beta, x1)

  list(
       "Expected Value (for X): E(Y|X)" = ev1,
       "Predicted Value (for X): E(Y|X)" = NA,
       "Expected Value (for X1): E(Y|X1)" = ev2,
       "Predicted Value (for X): E(Y|X)" = NA,
       "First Differences: E(Y|X1)-E(Y|X)" = ev2 - ev1
       )
}

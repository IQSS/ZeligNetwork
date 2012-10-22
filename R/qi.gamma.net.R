#' @S3method qi gamma.net
qi.gamma.net <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  
  inv <- linkinv(param)
  beta <- coef(param)
  alpha <- alpha(param)

  # Compute Expected Values
  #
  # ...
  # @param simulations ...
  # @param alpha ...
  # @param x
  # @return ...
  compute.ev <- function (simulations, alpha, x) {

    # Ensure 'setx' object is valid
    if (is.null(x) || is.na(x))
      return(NA)

    # Construct eta, the value of the linear predictors before the 
    # inverse-link function is applied
    eta <- beta %*% t(x)

    # Construct theta, the f^-1(eta)
    theta <- matrix(inv(eta), nrow=nrow(beta))

    # Size-up a matrix for "ev" the expected values of the simulation
    ev <- matrix(NA, nrow(theta), ncol(theta))

    # Properly name the matrix dimensions
    dimnames(theta) <- dimnames(ev) <- dimnames(eta)

    # Scale according to ancillary parameters
    ev <- theta / alpha

    # Return
    ev
  }

  ev1 <- compute.ev(beta, alpha, x)
  ev2 <- compute.ev(beta, alpha, x1)

  colnames(ev1) <- 1:ncol(ev1)
  rownames(ev1) <- 1:nrow(ev1)

  list(
       "Expected Value (for X): E(Y|X)" = ev1,
       "Expected Value (for X1): E(Y|X1)" = ev2,
       "First Differences: E(Y|X1) - E(Y|X)" = ev2-ev1
       )
}

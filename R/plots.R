.plot.net <- function (x, ...) {
  # Save old state
  old.par <- par(no.readonly=T)

  # Quantities of Interest
  qi <- x$qi

  # Define Relevant quantity of interest titles that have special properties
  ev.titles <- c('Expected Values: E(Y|X)', 'Expected Values (for X1): E(Y|X1)')
  pv.titles <- c('Predicted Values: Y|X', 'Predicted Values (for X1): Y|X1')

  # Determine whether two "Expected Values" qi's exist
  both.ev.exist <- all(ev.titles %in% names(qi))

  # Color of x should always be this pertty blue
  color.x <- rgb(242, 122, 94, maxColorValue=255)
  color.x1 <- rgb(100, 149, 237, maxColorValue=255)

  # This mixes the above two colors, and converts the result into hexadecimal
  color.mixed <- rgb(t(round((col2rgb(color.x) + col2rgb(color.x1))/2)), maxColorValue=255)

  if (is.null(x$x)) {
    return(par(old.par))
  }
  else if (is.null(x$x1) || is.na(x$x1)) {
    panels <- matrix(1, 1, 1)

    # The plotting device:
    # +--------+
    # |   1    |
    # +--------+
  }
  else {
    panels <- matrix(c(1, 2, 3, 3), ncol=2, nrow=2, byrow = TRUE)

    if (both.ev.exist)
      panels <- rbind(panels, c(3, 3, 4, 4))


    # The plotting device:
    #
    # +-----------+    +-----------+
    # |  1  |  2  |    |  1  |  2  |
    # +-----+-----+ OR +-----+-----+
    # |     3     |    |     3     |
    # +-----------+    +-----------+
    # |     4     |
    # +-----------+
  }

  #
  layout(panels)

  titles <- list(
    ev  = "Expected Value (for X): E(Y|X)",
    ev1 = "Expected Value (for X1): E(Y|X1)",
    fd  = "First Differences: E(Y|X1)-E(Y|X)"
    )


  Zelig:::simulations.plot(qi[[titles$ev]], main = titles$ev, col = color.x, line.col = "black")

  if (!is.null(x$x1) && !is.na(x$x1)) {
    Zelig:::simulations.plot(qi[[titles$ev1]], main = titles$ev, col = color.x1, line.col = "black")
    Zelig:::simulations.plot(qi[[titles$fd]], main = titles$fd, col = color.mixed, line.col = "black")
  }

  if (both.ev.exist) {
    Zelig:::simulations.plot(
      qi[["Expected Values: E(Y|X)"]],
      qi[["Expected Values (for X1): E(Y|X1)"]],
      main = "Comparison of E(Y|X) and E(Y|X1)",
      col = c(color.x, color.x1),
      line.col = "black")
  }

  # Restore old state
  par(old.par)

  # Return old parameter invisibly
  invisible(old.par)
}

#' @S3method plot sim.cloglog.net
plot.sim.cloglog.net <- .plot.net

#' @S3method plot sim.gamma.net
plot.sim.gamma.net <- .plot.net

#' @S3method plot sim.logit.net
plot.sim.logit.net <- .plot.net

#' @S3method plot sim.ls.net
plot.sim.ls.net <- .plot.net

#' @S3method plot sim.normal.net
plot.sim.normal.net <- .plot.net

#' @S3method plot sim.poisson.net
plot.sim.poisson.net <- .plot.net

#' @S3method plot sim.probit.net
plot.sim.probit.net <- .plot.net

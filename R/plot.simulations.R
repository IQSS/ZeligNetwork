#' @S3method plot sim.cloglog.net
plot.sim.cloglog.net <- function (x, ...) {
  # Save old state
  old.par <- par(no.readonly=T)

  # Quantities of Interest
  qi <- x$qi

  # Define Relevant quantity of interest titles that have special properties
  ev.titles <- c('Expected Values: E(Y|X)', 'Expected Values (for X1): E(Y|X1)')
  pv.titles <- c('Predicted Values: Y|X', 'Predicted Values (for X1): Y|X1')

  # Determine whether two "Expected Values" qi's exist
  both.ev.exist <- all(ev.titles %in% names(qi))
  # Determine whether two "Predicted Values" qi's exist
  both.pv.exist <- all(pv.titles %in% names(qi))

  # Color of x should always be this pertty blue
  color.x <- rgb(242, 122, 94, maxColorValue=255)
  color.x1 <- rgb(100, 149, 237, maxColorValue=255)

  # This mixes the above two colors, and converts the result into hexadecimal
  color.mixed <- rgb(t(round((col2rgb(color.x) + col2rgb(color.x1))/2)), maxColorValue=255)

  if (is.null(x$x)) {
    return(par(old.par))
  }
  else if (is.null(x$x1) || is.na(x$x1)) {
    panels <- matrix(1:2, 2, 1)

    # The plotting device:
    # +--------+
    # |   1    |
    # +--------+
    # |   2    |
    # +--------+
  }
  else {

    panels <- matrix(c(1:5, 5), ncol=2, nrow=3, byrow = TRUE)

    panels <- if (xor(both.ev.exist, both.pv.exist))
      rbind(panels, c(6, 6))
    else if (both.ev.exist && both.pv.exist)
      rbind(panels, c(6, 7))
    else
      panels


    # the plotting device:
    #
    # +-----------+    +-----------+
    # |  1  |  2  |    |  1  |  2  |
    # +-----+-----+    +-----+-----+
    # |  3  |  4  |    |  3  |  4  |
    # +-----+-----+ OR +-----+-----+
    # |     5     |    |     5     |
    # +-----------+    +-----------+
    # |  6  |  7  |    |     6     |
    # +-----+-----+    +-----+-----+
  }

  #
  layout(panels)

  titles <- list(
    ev  = "Expected Value (for X): E(Y|X)",
    ev1 = "Expected Value (for X1): E(Y|X1)",
    fd  = "First Differences: E(Y|X1)-E(Y|X)"
    )

  message("I KNOW YOU ARE THE WORST THING IN THE WORLD <<<")
  print(length(qi[[titles$ev]]))
  print(length(qi[[titles$ev1]]))
  print(length(qi[[titles$fd]]))
  # plot(qi[[titles$pv]], main = titles$ev, col = color.x, line.col = "black")
  # Zelig:::simulations.plot(qi[[titles$ev1]], main = titles$ev, col = color.x, line.col = "black")
  #Zelig:::simulations.plot(qi[[titles$fd]], main = titles$fd, col = color.mixed, line.col = "black")
  message(">>>")

  q()


  if (both.pv.exist) {
    simulations.plot(
      qi[["Predicted Values: Y|X"]],
      qi[["Predicted Values (for X1): Y|X1"]],
      main = "Comparison of Y|X and Y|X1",
      col = c(color.x, color.x1),
      line.col = "black")
  }

  if (both.ev.exist) {
    simulations.plot(
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

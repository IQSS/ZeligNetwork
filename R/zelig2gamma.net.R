#' Interface between the Zelig Model gamma.net and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2gamma.net <- function (formula, LF = "inverse", ..., data) {
  list(
       .function = "netgamma",
       formula = formula,
       data = data,
       LF = LF,
       ...
       )
}

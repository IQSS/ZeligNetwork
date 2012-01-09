#' Interface between the Zelig Model gamma.net and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2gamma.net <- function (formula, ..., data) {
  list(
       .function = "gamma.net",
       formula = formula,
       data = data,
       ...
       )
}

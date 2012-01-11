#' Interface between the Zelig Model poisson.net and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2poisson.net <- function (formula, ..., data) {
  list(
       .function = "poisson.net",
       formula = formula,
       data = data,
       ...
       )
}

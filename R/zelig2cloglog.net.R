#' Interface between the Zelig Model cloglog.net and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2cloglog.net <- function (formula, ..., data) {
  list(
       .function = "",
       formula = formula,
       data = data
       )
}

#' Interface between the Zelig Model ls.net and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2ls.net <- function (formula, ..., data) {
  list(
       .function = "",
       formula = formula,
       data = data
       )
}

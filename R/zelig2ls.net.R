#' Interface between the Zelig Model ls.net and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param LF a link function
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2ls.net <- function (formula, ..., LF=NULL, data) {
  list(
       .function = "normal.net",
       formula = formula,
       data = data,
       LF = LF,
       ...
       )
}

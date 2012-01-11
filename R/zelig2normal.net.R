#' Interface between the Zelig Model normal.net and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2normal.net <- function (formula, ..., data) {
  list(
       .function = "normal.net",
       formula = formula,
       data = data,
       ...
       )
}

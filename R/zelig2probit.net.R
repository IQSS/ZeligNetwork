#' Interface between the Zelig Model probit.net and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param LF a link function. This parameter is not allowed for cloglog
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2probit.net <- function (formula, LF="probit", ..., data) {
  # Link function warning
  if (!missing(LF))
    warning("Specifying the link-function (\"LF\") is ignored for ",
           "the \"probit.net\" model")

  # Return
  list(
       .function = "netbinom",
       formula = formula,
       LF = "probit",
       data = data,
       ...
       )
}

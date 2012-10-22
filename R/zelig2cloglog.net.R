#' Interface between the Zelig Model cloglog.net and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param LF a link function. This parameter is not allowed for cloglog
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2cloglog.net <- function (formula, ..., LF=NULL, data) {

  # Link function warning
  if (!missing(LF))
    warning("Specifying the link-function (\"LF\") is ignored for ",
           "the \"cloglog.net\" model")

  list(
       .function = "netbinom",
       formula = formula,
       LF = "cloglog",
       data = data,
       ...
       )
}

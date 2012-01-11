#' Interface between the Zelig Model logit.net and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2logit.net <- function (formula, LF="logit", ..., data) {

  # Link function warning
  if (!missing(LF))
    warning("Specifying the link-function (\"LF\") is ignored for ",
           "the \"logit.net\" model")

  # Return
  list(
       .function = "netbinom",
       formula = formula,
       LF = "logit",
       data = data,
       ...
       )
}

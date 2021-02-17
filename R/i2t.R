#' Inverse Logit transformation
#'
#' @param x an integer between -1 and 1
#' @keywords internal
#' @export

i2t <- function(x) 2/(1+exp(-x))-1

#' Logit transformation
#'
#' @param x an integer between -1 and 1
#' @keywords internal
#' @export
t2i <- function(x) log((1+x)/(1-x))





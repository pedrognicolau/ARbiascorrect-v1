#' Computes hermite polynomial of order k<5
#' @keywords internal
#' @export

hermite <- function(x, k) {
  if (k == 0)
    return (1)
  else if (k == 1)
    return (x)
  else if (k == 2)
    return (x^2-1)
  else if (k == 3)
    return (x*(x^2-3))
  else if (k == 4) {
    z <- x^2
    return (z*(z-6)+3)
  } else {
    stop("Order k < 5")
  }
}

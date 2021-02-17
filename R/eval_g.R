#' Evaluates the sum of the Hermite polynomials (ar1)
#' @keywords internal
#' @export

eval.g.ar1 = function(x, beta) {
  val <- 0
  for(k in seq_along(beta))
    val = val + beta[k] * hermite(x, k-1)/sqrt(factorial(k-1))
  return (val)
}

#' Find product of Hermite polynomials in two-dimensional case
#' @keywords internal
#' @export

g = function(x, k) {
  G <- f_order(2)
  kk <- G$order.map[[k]]
  scaling <- 1/sqrt(prod(factorial(kk)))
  return (hermite(x[, 1], kk[1]) * hermite(x[, 2], kk[2]) * scaling)
}

#' Evaluate sum of Hermite polynomials for g(pacf1) and g(pacf2)
#' @keywords internal
#' @export

eval.g.ar2 = function(x, beta) {
  if (!is.matrix(x)) x <- matrix(x, ncol = 2)
  val1 <- val2 <- 0
  for(k in 1:10) {
    gg <- g(x, k)
    val1 = val1 + beta[k] * gg
    val2 = val2 + beta[k + 10] * gg # G$KK=10
  }
  return (cbind(val1, val2))
}

#' Evaluate separate regression model for each of the SN-parameters + rho
#' @keywords internal
#' @export

eval.g.sn = function(x, beta) {
  G <- f_order(2)
  if (!is.matrix(x)) x <- matrix(x, ncol = G$npar)
  val1 <- 0
  for(k in 1:G$KK) {
    gg <- g(x, k)
    val1 = val1 + beta[k] * gg
  }
  return (val1)
}


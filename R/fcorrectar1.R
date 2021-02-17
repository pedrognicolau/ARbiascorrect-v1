#' @importFrom fGarch rsnorm
NULL

#' Corrects the bias of autoregressive coefficients for AR(1)
#'
#' @param phi integer for the autoregressive coeeficient estimate
#' @param n integer for the length of the time series. Needs to be between 10 and 50.
#' @param method character string specifying the used method to estimate the autoregressive coefficients. Needs to be one of 'yw', 'mle', 'burg', 'cmle'
#' 'yw': Yule-Walker, 'mle': Maximum Likelihood, 'burg': Burg's algorithm, 'cmle': Conditional Maximum Likelhood")
#' @param x vector for the time series for which parameters are to be estimated, in case no previous algorithm has been made.
#' The default method to estimate is mle if none is specified.
#' @return The bias corrected \code{phi1} and associated confidence intervals.
#' @keywords internal
#' @export



f.correct.ar1 = function(phi = NULL, method = NULL, n = NULL, x = NULL){
  # if time series is given:
  if (!is.null(x)){
    n = length(x)
    phi = f.orig(x, method, 1)
  }

  G <- f_order(1)
  b <- ar1_coefs
  sn <- ar1_skew
  quant <- c(0.025,0.975)
  no.mc <- 10000

  # 1. Compute bias corrected estimate, K=3
  beta = as.numeric(b[b$n==n & b$method==method,4:7])
  phi.correct = i2t(eval.g.ar1(t2i(phi), beta))

  # 2. Find skew-normal approximation, K=3
  sn.beta = as.numeric(sn[sn$n==n & sn$method==method, 3:14])
  theta = rep(0,G$sn)
  for (s in 1:G$sn) theta[s] = eval.g.ar1(t2i(phi),sn.beta[(G$KK*(s-1)+1):(G$KK*s)])

  # Sampling
  mc = rsnorm(no.mc, mean = theta[1], sd = theta[2], xi = exp(theta[3]))
  mc.orig = i2t(mc)
  mc.correct = i2t(eval.g.ar1(mc,beta))

  # Confidence interval estimates
  conf.int = c(quantile(mc.orig, prob = quant), quantile(mc.correct, prob = quant))

  return(list(phi.hat = phi, phi.correct = phi.correct, ci.hat = conf.int[1:2], ci.correct = conf.int[3:4]))
}

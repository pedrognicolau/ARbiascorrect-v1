#' Corrects the bias of autoregressive coefficients for AR(2)
#'
#' @param phi vector for the autoregressive coeeficients estimates
#' @param n integer for the length of the time series. Needs to be between 10 and 50.
#' @param method character string specifying the used method to estimate the autoregressive coefficients. Needs to be one of 'yw', 'mle', 'burg', 'cmle'
#' 'yw': Yule-Walker, 'mle': Maximum Likelihood, 'burg': Burg's algorithm, 'cmle': Conditional Maximum Likelhood")
#' @param x vector for the time series for which parameters are to be estimated, in case no previous algorithm has been made.
#' The default method to estimate is mle if none is specified.
#' @return The bias corrected \code{phi} and associated confidence intervals.
#' @keywords internal
#' @export

f.correct.ar2 = function(phi = NULL, method = NULL, n = NULL, x = NULL){
  # if time series is given:

  if (!is.null(x)){
    n = length(x)
    phi = f.orig(x, method, 2)
  }

  pacf = ar.phi2pacf(phi)

  G <- f_order(2)
  b <- ar2_coefs
  sn <- ar2_skew
  quant <- c(0.025,0.975)

  # 1. Compute bias corrected estimate, K=3
  beta = as.numeric(b[b$n==n & b$method==method,7:26])
  phi.correct = ar.pacf2phi(i2t(eval.g.ar2(t2i(pacf), beta)))

  # 2. Find skew-normal approximation, K=3
  sn.beta = as.numeric(sn[sn$n==n & sn$method==method, 3:72])

  theta = rep(0,G$sn)
  for (s in 1:G$sn) theta[s] = eval.g.sn(t2i(pacf),sn.beta[(G$KK*(s-1)+1):(G$KK*s)])
  x.sn =  f.copula(theta)

  # Sample for original pacf-estimates (pacf1,pacf2)
  pacf.sim = i2t(x.sn)

  # Sample for original phi-estimates (phi1, phi2)
  phi.sim = pacf2phi(pacf.sim)

  # Sample for original and corrected phi
  phi.sample = cbind(phi.sim, pacf2phi(i2t(eval.g.ar2(x.sn, beta))))

  # Quantiles for original (phi1, phi2) and corrected (phi1c, phi2c)
  conf.int = rep(0, G$npar*4)
  for (k in 1:4) conf.int[(2*(k-1)+1):(2*k)] =  as.numeric(quantile(phi.sample[,k],quant))
  return(list(phi.hat = phi, phi.correct = phi.correct, ci.hat= conf.int[1:4], ci.correct = conf.int[5:8]))
}

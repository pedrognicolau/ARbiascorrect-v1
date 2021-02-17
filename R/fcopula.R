#' @importFrom fGarch dsnorm psnorm
#' @importFrom MASS mvrnorm
#' @importFrom stats ar pnorm splinefun quantile
#' @import FitAR
NULL


#' Find samples using Copula approach to preserve correlation
#'
#' @keywords internal
#' @export

f.copula = function(theta.vec){

  no.mc <- 10000
  x.seq <- seq(-30,30,0.01)

  theta.r = c(theta.vec[1], theta.vec[2], exp(theta.vec[3]),
              theta.vec[4], theta.vec[5], exp(theta.vec[6]),
              i2t(theta.vec[7]))
  d = fGarch :: dsnorm(x.seq, mean = theta.r[1], sd = theta.r[2], xi = theta.r[3])
  x.r = x.seq[d > 0.01]
  a1 = fGarch :: psnorm(x.r, mean = theta.r[1], sd = theta.r[2],  xi = theta.r[3])
  f1 = splinefun(a1, x.r, ties=min)

  d = fGarch :: dsnorm(x.seq, mean = theta.r[4], sd = theta.r[5], xi = theta.r[6])
  x.r = x.seq[d > 0.01]
  a2 = fGarch :: psnorm(x.r, mean = theta.r[4], sd = theta.r[5],  xi = theta.r[6])
  f2 = splinefun(a2, x.r, ties=min)

  rho = theta.r[7]
  #for (i in c(2,5)) theta[i]=sqrt(1/exp(theta[i]))
  w = mvrnorm(no.mc,c(0,0),matrix(c(1,rho,rho,1),ncol=2))
  u = cbind(pnorm(w[,1]),pnorm(w[,2]))

  # Sample for g(pacf1), g(pacf2) based on skew-normal distribution and copula representation
  # Error in qsnorm!!!!
  #x.sn = cbind(qsnorm(u[,1], mean = theta.r[1], sd = theta.r[2], xi = theta.r[3]),
  #             qsnorm(u[,2], mean = theta.r[4], sd = theta.r[5], xi = theta.r[6]))

  x.sn = cbind(f1(u[,1]),f2(u[,2]))
  return(x.sn)
}


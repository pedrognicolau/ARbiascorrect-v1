#' AR Bias Correction for short time series
#'
#' Gives bias-corrected estimates and 95\% confidence intervals for autoregressive coefficients of  AR(1) and AR(2) processes,
#' for sample sizes n=10,11, \ldots, 50.
#'
#' @param phi single value (AR1) or two-dimensional vector (AR2) containing the AR estimates subject to bias correction. Not required
#' if time series \code{x} is used as input.
#' @param n integer for the length of the time series. Needs to be between 10 and 50. Not required
#' if time series \code{x} is used as input.
#' @param method Character string specifying the method used to estimate the autoregressive
#' coefficients. Needs to be either \code{mle}, \code{cmle}, \code{burg} or \code{yw},
#' specifying the exact MLE, the conditional MLE, Burg's method and the Yule-Walker solution,
#' respectively.
#' @param x The time series to be fitted by AR(1) or AR(2) in which \code{order} needs to be specified.
#' The default estimation method is the exact MLE if none is specified.
#' @param order order of the estimated autoregressive process. \code{x} is used as input.
#' @return A list with the following elements
#' \itemize{
#' \item \code{phi.hat} The original estimates of the AR coefficients
#' \item \code{phi.correct} The bias-corrected estimates
#' \item \code{ci.hat} The 95\% confidence interval for the original estimates
#' \item \code{ci.correct} The 95\% confidence interval for the bias-corrected estimates
#' }
#'
#' @examples
#' # pre-computed estimates
#' biascorrect(phi=c(0.5,-0.2), n=15, method="mle")
#'
#'
#' \donttest{
#' # simulating series with ARIMA sim
#' series <- arima.sim(n = 20, list(ar = c(0.2, 0.5), sd = sqrt(0.04)))
#' biascorrect(x=series, order=2, method="mle")
#' }
#'
#' @references
#' Sørbye, S. H., Nicolau, P. G. & Rue, H. (2021). Finite-sample properties of estimators for first
#' and second order autoregressive processes.
#'
#' @export
biascorrect <- function(phi=NULL,n=NULL, method=c("yw","mle","burg","cmle"),order=NULL, x=NULL)
{

  # if order is not added
  if(is.null(order)==TRUE) order <- length(as.numeric(phi))

  # if time series is provided
  if(is.null(x)==FALSE)
    {
    # compute n from length
    n <- length(x)
  }

  # add default method
  if(length(method)>1)
    print("default method: MLE")
  method <- "mle"

  # check for common errors
  print_errors(phi,n,method,order,x)

  # AR(1)
  if(order==1)
  {
    result <- f.correct.ar1(phi, method, n, x)
  }

  # AR(2)
  if(order==2)
  {
    result <- f.correct.ar2(phi, method, n, x)
  }

  # return result
  return(result)

}


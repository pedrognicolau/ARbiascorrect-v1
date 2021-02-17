#' Find original estimate for coefficients if time series is given
#'
#' @keywords internal
#' @export

f.orig = function(x, method, p){
  if(is.null(method)==TRUE) method <- "mle"
  if (method=="mle") phi = FitAR::FitAR(x, p = p, MeanMLE=T)$phiHat
  if (method=="yw")  phi = ar(x, aic = F, order.max = p, method = "yw")$ar
  if (method=="burg") phi = ar(x, aic = F, order.max = p, method = "burg")$ar
  if (method=="cmle") phi = ar(x, aic = F, order.max = p, method = "mle")$ar
  return(phi)
}

#' Convert partial autocorrelation coefficients to autoregressive coeffients
#'
#' @param pac partial autocorrelation coefficient (vector)
#' @keywords internal
#' @export

ar.pacf2phi = function(pac) {
  p = length(pac)
  phi = work = pac
  if (p > 1L)
    for (j in 1L:(p-1L)) {
      a = phi[j+1L];
      phi[1L:j] = work[1L:j] = work[1L:j] - a * phi[j:1L]
    }
  return(phi)
}

#' Convert autoregressive coeffients to partial autocorrelation coefficients
#'
#' @param pac autoregressive coefficients (vector)
#' @keywords internal
#' @export

ar.phi2pacf = function(phi) {
  p = length(phi)
  work = pac = phi
  if (p > 1L)
    for(j in (p-1L):1L) {
      a = pac[j+1L];
      pac[1L:j] = work[1L:j]  = (pac[1L:j] + a * pac[j:1L]) / (1.0 - a^2);
    }
  return (pac)
}

#' Apply ar.pacf2phi to pacf matrix
#'
#' @param pac.mat matrix
#' @keywords internal
#' @export
pacf2phi <- function(pac.mat) t(apply(pac.mat, 1, ar.pacf2phi))

#' Apply ar.phi2pacf to pacf matrix
#'
#' @param pac.mat matrix
#' @keywords internal
#' @export
phi2pacf <- function(pac.mat) t(apply(pac.mat, 1, ar.phi2pacf))



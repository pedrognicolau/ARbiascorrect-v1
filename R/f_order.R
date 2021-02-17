#' AR(1) and AR(2): Defining map etc for K=3 order regression
#' @keywords internal
#' @export
#'
f_order = function(p){
  G <- list(target = 1, val.reference = 1)
  G$K <- 3
  G$order.map <- list(list())
  k <- 0

  if (p==1){
    G$npar <-1
    for(i in 0:G$K) {
      k=k+1
      G$order.map[[k]] <- i
    }
    G$KK <- k
    G$sn <- p*3
  }

  if (p==2){
    G$npar <-2
    for(i in 0:G$K) {
      for(ii in 0:G$K) {
        if (i+ii <= G$K) {
          k <- k+1
          G$order.map[[k]] <- c(i, ii)
        }
      }
    }
    G$KK <- k
    G$sn <- p*3+1
  }
  return(G)
}

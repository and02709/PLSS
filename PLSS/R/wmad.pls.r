#' @param Y
#' @param Yhat
#' @export

wmad.pls <- function(Y,Yhat){
  y.means <- colMeans(abs(Y))
  mat <- abs(Y-Yhat)
  evec <- rep(1,nrow(Y))
  mat.2 <- tcrossprod(evec,Y.means)
  y.vec <- colmeans((mat/mat.2))
  return(mean(y.vec))
}
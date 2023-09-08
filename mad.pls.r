#' @param Y
#' @param Yhat
#' @export

mad.pls <- function(Y,Yhat){
  mat <- abs(Y-Yhat)
  y.vec <- colMeans(mat)
  return(mean(y.vec))
}
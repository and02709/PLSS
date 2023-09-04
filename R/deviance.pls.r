#' @param Y
#' @param Yhat
#' @export

deviance.pls <- function(Y,Yhat){
  dev.vec <- PLSS::deviance.calc(Yhat,Y)
  return(mean(dev.vec))
}

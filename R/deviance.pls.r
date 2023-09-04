#' @param Y
#' @param Yhat
#' @export

deviance.pls <- function(Y,Yhat){
  dev.vec <- plss::deviance.calc(Yhat,Y)
  return(mean(dev.vec))
}
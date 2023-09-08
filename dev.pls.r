#' This calculates deviance over the Y Yhat
#' @param Y
#' @param Yhat
#' @export
#' @example 

dev.pls <- function(Y,Yhat){
  dev.vec <- PLSS::dev.calc(Yhat,Y)
  return(mean(dev.vec))
}

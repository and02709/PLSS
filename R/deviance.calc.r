#' @param E
#' @param O
#' @export

deviance.calc <- function(E,O){
  n <- nrow(E)
  #deviance.vec <- rep(0,n)
  M <- 2*O*log(O/E)
  M[is.nan(M)] <- 0
  temp <- apply(M,1,sum)
  return(temp)
}
#' @param X
#' @param Y
#' @param ncomp
#' @export

pls.svd <- function(X,Y,ncomp){
  M <- Rfast::Crossprod(X,Y)
  M.decomp <- RSpectra::svds(M,k=ncomp)
  return(list(M=M,u=M.decomp$u,v=M.decomp$v))
}
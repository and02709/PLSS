#' @param X
#' @param Y
#' @param n.lam1
#' @param n.lam2
#' @param eps
#' @export

uv.max.step <- function(X,Y,n.lam1,n.lam2,eps){
  temp.svd <- PLSS::pls.svd(X,Y,1)
  max.u <- max(abs(temp.svd$u))-eps
  max.v <- max(abs(temp.svd$v))-eps
  step.u <- max.u/n.lam1
  step.v <- max.v/n.lam2
  lam1 <- seq(from=0,to=max.u,by=step.u)
  lam2 <- seq(from=0,to=max.v,by=step.v)
  return(list(lam1=lam1,lam2=lam2))
}

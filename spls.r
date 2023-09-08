#' @param X
#' @param Y
#' @param ncomp
#' @param mode
#' @param lam1
#' @param lam2
#' @param trace
#' @param niter
#' @param threshold
#' @export

spls <- function(X,Y,ncomp,mode=NULL,lam1=NULL,lam2=NULL,trace=F,niter=200,threshold=1e-6){
  Xi <- X
  Yi <- Y
  U <- matrix(0, nrow=ncol(X), ncol=ncomp)
  V <- matrix(0, nrow=ncol(Y), ncol=ncomp)
  for(i in 1:ncomp){
    uv.temp <- PLSS::pls.svd(Xi,Yi,1)
    u <- uv.temp$u
    v <- uv.temp$v
    if(!is.null(lam1) || !is.null(lam2)){
      if(is.null(lam1)) lam1 <- 0
      if(is.null(lam2)) lam2 <- 0
      uv.temp <- PLSS::ssvd(M=uv.temp$M,u=u,v=v,lam1=lam1,lam2=lam2,trace=trace,niter=niter,threshold=threshold,ncomp=i)
      u <- uv.temp$u
      v <- uv.temp$v
    }
    if(l2n(u)==0 || l2n(v)==0){
      warning("Resulting matrices have norms of zero")
      if(i==1) return(list(U=U[,1],V=V[,1]))
      return(list(U=U[,1:(i-1)],V=V[,1:(i-1)]))
    }
    U[,i] <- u
    V[,i] <- v
    if(i==ncomp) break
    xy.temp <- PLSS::pls.deflate(X=Xi,Y=Yi,u=u,v=v,mode)
    Xi <- xy.temp$Xi
    Yi <- xy.temp$Yi
  }
  return(list(U=U,V=V))
}

#' @param M
#' @param u
#' @param v
#' @param lam1
#' @param lam2
#' @param trace
#' @param niter
#' @param threshold
#' @param ncomp
#' @export

ssvd <- function(M,u,v,lam1,lam2,trace,niter,threshold,ncomp){
  #M <- Rfast::Crossprod(X,Y)
  uold <- rnorm(length(u),0,1)
  vold <- rnorm(length(v),0,1)
  iter <- 1
  if(trace) cat("component ",ncomp,": ")
  while(plss::l2n(u-uold) > threshold && plss::l2n(v-vold) > threshold ){
    if(trace) cat(iter," ")
    #u <- soft(M%*%v,lam1)
    l2n.v <- plss::l2n(v)
    if(l2n.v==0){
      u <- rep(0, length(u))
      break
    }
    u <- M%*%v
    u <- u/plss::l2n(u)
    u <- plss::soft(u,lam1)
    l2n.u <- plss::l2n(u)
    ifelse(l2n.u==0, u<-u, u<-u/plss::l2n(u))
    
    #v <- soft(crossprod(M,u),lam2)
    if(l2n.u==0){
      v <- rep(0,length(v))
      break
    }
    v <- crossprod(M,u)
    v <- v/plss::l2n(v)
    v <- plss::soft(v,lam2)
    l2n.v <- plss::l2n(v)
    ifelse(l2n.v==0, v<-v, v<-v/plss::l2n(v))
    if(iter==niter) break
    iter <- iter+1
    uold <- u
    vold <- v
  }
  cat("\n")
  return(list(u=u,v=v))
}
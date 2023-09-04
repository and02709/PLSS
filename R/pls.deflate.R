#' @param X
#' @param Y
#' @param u
#' @param v
#' @param mode
#' @export

pls.deflate <- function(X,Y,u,v,mode){
  zeta <- X%*%u/as.double(crossprod(u,u))
  c <- crossprod(X,zeta)/as.double(crossprod(zeta,zeta))
  Xi <- X - tcrossprod(zeta,c)
  if(mode==2){
    d <- crossprod(Y,zeta)/as.double(crossprod(zeta,zeta))
    Yi <- Y - tcrossprod(zeta,d)
  } else{
    omega <- Y%*%v/as.double(crossprod(v,v))
    e <- crossprod(Y,omega)/as.double(crossprod(omega,omega))
    Yi <- Y - tcrossprod(omega,e)
  }
  return(list(Xi=Xi, Yi=Yi)) 
}
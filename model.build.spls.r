#' @param Y
#' @param X
#' @param U 
#' @param Y.colnames
#' @export

model.build.spls <- function(Y,X,U,Y.colnames){
  if(norm(U)==0){
    colnames(Y) <- Y.colnames
    mod <- lm(Y~1)
  } else{
    Z <- data.frame(Rfast::mat.mult(X,U))
    n.comp <- dim(Z)[[2]]
    # colnames(Z) <- paste0("Z",1:ncomp)
    # colnames(Y) <- Y.colnames
    df <- data.frame(Y,Z)
    Z.colnames <- paste0("Z",1:n.comp)
    colnames(df) <- c(Y.colnames,Z.colnames)
    lm.structure <- paste(paste("cbind(",paste(Y.colnames, collapse= ","),")"), paste(Z.colnames, collapse= " + "), sep= " ~ ")
    mod <- lm(lm.structure,data=df)
  }
  return(mod)
}
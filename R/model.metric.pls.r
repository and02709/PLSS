#' @param Y
#' @param X
#' @param mod 
#' @param U 
#' @param metric
#' @param ncomp
#' @param Y.colnames
#' @export

model.metric.pls <- function(Y,X,mod,U,metric=c("MAD","WMAD","Deviance"),ncomp,Y.colnames){
  if(norm(U)==0){
    colnames(Y) <- Y.colnames
    Yhat <- pedict(mod,data.frame(Y)) |> as.matrix()
  } else{
    Z <- data.frame(Rfast::mat.mult(X,U)) 
    Z.colnames <- paste0("Z",1:ncomp)
    colnames(Z) <- Z.colnames
    #colnames(Z) <- paste("z",1:ncomp)
    #colnames(Y) <- Y.colnames
    #df <- data.frame(Y,Z)
    Yhat <- predict(mod,Z) |> as.matrix()
  }
  
  metric <-  switch(metric,
                    MAD={PLSS::mad.pls(Y,Yhat)},
                    WMAD={PLSS::wmad.pls(Y,Yhat)},
                    Deviance={PLSS::deviance.pls(Y,Yhat)}
  )
  return(metric)
}

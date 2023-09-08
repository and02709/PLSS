#' @param arg.vec
#' @param df.partition
#' @param ncomp
#' @param mode
#' @param Y.colnames
#' @param trace
#' @param niter 
#' @param threshold
#' @param metric
#' @export

cv.partition.spls <- function(arg.vec, df.partition, ncomp, mode,Y.colnames,trace,niter,threshold,metric){
  test.index <- arg.vec[[1]]
  lam1 <- arg.vec[[2]]
  lam2 <- arg.vec[[3]]
  
  if(trace) cat("Test Index: ",test.index, ", lambda 1: ",lam1, ", lambda 2: ",lam2,"\n")
  #if(test.index==2 && lam1==0 && lam2>0.622944) browser()
  
  xtrain <- df.partition |> dplyr::filter(.folds!=test.index) |> dplyr::ungroup() |> dplyr::select(-c(Y.colnames,.folds)) |> as.matrix()
  xtest <- df.partition |> dplyr::filter(.folds==test.index) |> dplyr::ungroup() |> dplyr::select(-c(Y.colnames,.folds)) |> as.matrix()
  ytrain <- df.partition |> dplyr::filter(.folds!=test.index) |> dplyr::ungroup() |> dplyr::select(Y.colnames) |> as.matrix()
  ytest <- df.partition |> dplyr::filter(.folds==test.index) |> dplyr::ungroup() |> dplyr::select(Y.colnames) |> as.matrix()
  
  xmeans <- colMeans(xtrain)
  xtrain <- t(apply(xtrain, 1, function(x) x-xmeans))
  xtest <- t(apply(xtest, 1, function(x) x-xmeans))
  
  U <- PLSS::spls(X=xtrain,Y=ytrain,ncomp=ncomp,mode=mode,lam1=lam1,lam2=lam2,trace=trace,niter=niter,threshold = threshold)$U |> as.matrix()
  mod <- PLSS::model.build.spls(Y=as.matrix(ytrain),X=as.matrix(xtrain),U=U,Y.colnames=Y.colnames)
  metric <- PLSS::model.metric.pls(Y=as.matrix(ytest),X=as.matrix(xtest),mod=mod,U=U,metric=metric,Y.colnames = Y.colnames)
  return(metric)
  
}

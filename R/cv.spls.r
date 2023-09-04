#' @param X
#' @param Y
#' @param ncomp
#' @param mode
#' @param n.folds
#' @param n.lam1
#' @param n.lam2
#' @param X.colnames
#' @param Y.colnames
#' @param trace
#' @param niter
#' @param threshold
#' @param eps
#' @param parallel
#' @param n.cores
#' @param metric
#' @export

cv.spls <- function(X,Y,ncomp=1,mode=NULL,n.folds=5,n.lam1=NULL,n.lam2=NULL,X.colnames=NULL, Y.colnames=NULL, trace=F,niter=200,
                    threshold=1e-6,eps=1e-8,parallel=F,n.cores=NULL,metric=c("MAD","WMAD","Deviance")){
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  n <- nrow(X)
  p <- ncol(X)
  n.y <- nrow(Y)
  p.y <- ncol(Y)
  if(n!=n.y) stop("Rows do not match between predictors and response")
  if(mode!=1 && mode!=2) stop("Specify PLS mode 1 or 2")
  
  if(length(X.colnames)!=p) X.colnames <- NULL
  if(length(Y.colnames)!=p.y) Y.colnames <- NULL
  if(is.null(X.colnames)){
    X.colnames <- paste0("X",1:p)
    colnames(X) <- X.colnames
  } 
  if(is.null(Y.colnames)){
    Y.colnames <- paste0("Y",1:p.y)
    colnames(Y) <- Y.colnames
  } 
  if(is.null(n.lam1)) n.lam1 <- 100
  if(is.null(n.lam2)) n.lam2 <- 100
  if(n.lam1 < 3) stop("At least 3 values to be checked for lambda 1")
  if(n.lam2 < 3) stop("At least 3 values to be checked for lambda 2")
  
  df <- data.frame(Y,X)
  df.partition <- groupdata2::fold(data=df,k=n.folds)
  
  lam.lists <- plss::uv.max.step(X=X,Y=Y,n.lam1=n.lam1,n.lam2=n.lam2,eps=eps)
  lam1 <- lam.lists$lam1
  lam2 <- lam.lists$lam2
  n.lams <- length(lam1)*length(lam2)
  fold.arg <- c(1:n.folds)
  param.grid <- expand.grid(fold.arg,lam1,lam2)
  colnames(param.grid) <- c("fold.arg","lam1","lam2")
  
  if(parallel){
    if(is.null(n.cores)) n.cores <- parallel::detectCores()
    clust <- parallel::makeCluster(n.cores)
    metric.vec <- parallel::parApply(cl=clust,X=as.matrix(param.grid),1, cv.partition.spls, df.partition=df.partition, ncomp=ncomp, mode=mode,Y.colnames=Y.colnames,trace=trace,niter=niter,threshold=threshold,metric=metric)
  } else{
    metric.vec <- apply(X=as.matrix(param.grid),1, cv.partition.spls, df.partition=df.partition, ncomp=ncomp, mode=mode,Y.colnames=Y.colnames,trace=trace,niter=niter,threshold=threshold,metric=metric)
  }
  param.grid <- cbind(param.grid,metric.vec)
  metric.matrix <- matrix.fill(param.grid=param.grid,n.lams=n.lams,n.folds=n.folds)
  cv.metric <- rowMeans(metric.matrix)
  lam.list.vec <- expand.grid(lam1,lam2)
  cv.df <- data.frame(lam.list.vec,cv.metric)
  colnames(cv.df) <- c("lam1","lam2","cv.metric")
  best.metric <- min(cv.df$cv.metric)
  best.lam1 <- cv.df$lam1[which(cv.df$cv.metric==best.metric)]
  best.lam2 <- cv.df$lam2[which(cv.df$cv.metric==best.metric)]
  
  return(list(cv.matrix=metric.matrix, cv.metric=cv.df, lam1=best.lam1, lam2=best.lam2))
}
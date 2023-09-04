#' @param param.grid
#' @param n.lams
#' @param n.folds
#' @export

matrix.fill <- function(param.grid,n.lams,n.folds){
  #metric.matrix <- matrix(NA,nrow=n.lams,ncol = n.folds)
  back.set <- n.folds-1
  metric.matrix <- sapply(1:n.lams,function(x) param.grid[(x*n.folds-back.set):(x*n.folds),4])
  metric.matrix <- t(metric.matrix)
  return(metric.matrix)
}
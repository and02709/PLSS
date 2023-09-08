#' @param vec
#' @export

l2n <- function(vec){
  a <- sqrt(sum(vec^2))
  return(a)
}
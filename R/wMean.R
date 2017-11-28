#' Calculate a weighted average
#'
#' Function to calculate a weighted average.
#'
#' @param Y the values to calculate the weighted average over.
#' @param W the weights (of length Y or 1) to use in the weighted average
#'
#' @return the weighed average
#' @export
wMean <- function(Y,W){
  if((length(W) != 1) & (length(W) != length(Y))){
    stop('Vector of weights should be of length Y or 1')
  }
  M <- sum(W*Y)/sum(W)
  return(M)
}






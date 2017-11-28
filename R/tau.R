#' DerSimonian & Laird estimator for between-study variance.
#'
#' Function to estimate \eqn{\tau^2}, the DerSimonian & Laird (1986) estimator
#' for between-study variance. Usefull to execute a random effects meta-analysis.
#'
#' @param Y a vector with the estimates for the study effects
#' (e.g. standardized effect sizes Hedges' g).
#' @param W a vector with the study specific weights.
#' Most often the inverse of the variance of Y.
#' See also \code{\link{varHedge}}.
#' @param k amount of studies in the meta-analysis.
#'
#' @references \url{https://www.ncbi.nlm.nih.gov/pubmed/3802833}
#'
#' @return an estimate for between-study variance.
#' @export
tau <- function(Y,W,k){
  C <- sum(W)-(sum(W^2)/sum(W))
  df <- k-1
  Q <- sum(W*Y^2)-sum(W*Y)^2/sum(W)
  if(Q < df){
    T2 <- 0
  }else{
    T2 <- (Q-df)/C
  }
  return (T2)
}

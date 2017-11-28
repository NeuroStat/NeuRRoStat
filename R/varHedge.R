#' Calculate within-study variance of g
#'
#' Function to calculate the within-study variance of one sample
#' effect size g using calculations from Hedges (1989).
#' Input is the standardized effect size g and N.
#'
#' @param g standardized effect size g from one sample set-up.
#' @param N sample size
#'
#' @return var(g)
#' @export
varHedge <- function(g,N){
  value <- (1/N) + (1 - (gamma((N - 2) / 2) / gamma((N - 1) / 2))^2 * (N - 3) / 2) * g^2
  return(round(value,7))
}

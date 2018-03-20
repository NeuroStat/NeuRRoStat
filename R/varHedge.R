#' Calculate within-study variance of g
#'
#' Function to calculate the within-study variance of one sample
#' effect size g using the formula from Radua.
#' Input is the standardized effect size g and N.
#'
#' @param g standardized effect size g from one sample set-up.
#' @param N sample size
#'
#' @seealso \code{\link{varHedgeT}} for the function of the variance based on
#' the non-central t-distribution from which we know the derivations.
#' We do not know the derivations for the current expression from Radua.
#'
#' @return The estimate of the within-study variability of Hedges g.
#' @export
varHedge <- function(g,N){
  warning(
"This function will be deprecated, as we do not know the derivations for this expression.
  Use the function varHedgeT instead!")
  value <- (1/N) + (1 - (gamma((N - 2) / 2) / gamma((N - 1) / 2))^2 * (N - 3) / 2) * g^2
  return(round(value,7))
}

#' Calculate within-study variance of g based on non-central t
#'
#' This function calculates the variance of the one-sample standardized mean effect
#' size Hedges g. It is based on the derivations of Hedges (1984)
#' Input is the standardized effect size g and N, the study sample size.
#'
#' @param g standardized effect size g from one sample set-up.
#' @param N sample size
#'
#' @seealso \code{\link{varHedge}} for the function of the variance based on
#' the derivations from Radua.
#'
#' @return The estimate of the within-study variability of Hedges g.
#' @export
varHedgeT <- function(g,N){
  # The derivation is based on Cohen d, hence we transform g to d
  cohenD <- g/corrH(N)

  # Now calculate the variance
  numPartA <- ((N - 1) * (1 + N*cohenD**2))
  denomPartA <- N*(N - 3)

  numPartB <- cohenD**2
  denomPartB <- corrH(N)**2

  value <- ((numPartA/denomPartA) - (numPartB/denomPartB)) * corrH(N)**2
  return(round(value,7))
}

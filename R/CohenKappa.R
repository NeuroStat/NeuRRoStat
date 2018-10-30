#' Calculate Cohen's Kappa.
#'
#' Function to calculate a coherence incex called Cohen's \eqn{\kappa} which
#' gives an estimate of the amount of agreement between different raters.
#' Or the agreement on realizations of observations from a mixture of two
#' binomial distributions.
#'
#' This function is used to estimate the agreement of replication images in fMRI data.
#' Therefore, we denote the parameters that are obtained and described in \code{\link{EMbinom}}.
#'
#' @param lambda the proportion of truly activated voxels
#' @param piAI the probability that a truly active voxel is declared active
#' @param piI1 the probability that a truly inactive voxel is declared active
#'
#' @return Numeric with the index of coherence (\eqn{\kappa}).
#' @export
CohenKappa <- function(lambda, piA1, piI1){
  # Check if the values are within 0 and 1
  if(any(c(lambda, piA1, piI1) < 0 | c(lambda, piA1, piI1) > 1)) error('Values should be between 0 and 1!')

  # Now calculate the piA0 and piI0
  piA0 <- 1 - piA1
  piI0 <- 1 - piI1

  # Calculate the proportion of correct classifications (p0)
  p0 <- lambda*piA1 + (1 - lambda) * piI0

  ## Agreement by chance
  # pi1 or also denoted as tau
  pi1 <- lambda * piA1 + (1 - lambda) * piI1
  # pi0 or also given by 1 - tau
  pi0 <- 1 - pi1
  # pC
  pC <- lambda * pi1 + (1 - lambda) * pi0

  # Cohen's Kappa
  CohKap <- (p0 - pC)/(1 - pC)

  return(CohKap)
}

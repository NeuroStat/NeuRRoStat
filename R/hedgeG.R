#' Calculate Hedges' g from one sample t-test statistic.
#'
#' This function takes the t test-statistic from a \strong{one-sample t-test}
#' and calculates Hedges' g.
#'
#' @param t T test-statistic from a one-sample t-test
#' @param N Sample size.
#' @param type Use the approximation for the unbiased estimator (default)
#' or the exact expression.
#'
#' @details
#' The approximation is based on the correction factor \code{\link{corrJ}}.
#' The exact expression is based on \code{\link{corrH}}.
#'
#' @return Standardized mean effect size Hedges' g.
#' @export
hedgeG <- function(t,N, type = c('approx', 'exact')){
  # Check arguments
  type <- match.arg(type)

  # Approximation: using J
  if(type == 'approx'){
    G <- (t/sqrt(N))*corrJ(N)
  }

  # Exact expression: using h
  if(type == 'exact'){
    G <- (t/sqrt(N))*corrH(N)
  }

  # Return
  return(G)
}

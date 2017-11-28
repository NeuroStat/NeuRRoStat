#' Calculate Hedges' g from one sample t-test statistic.
#'
#' This function takes the t test-statistic from a \strong{one-sample t-test}
#' and calculates Hedges' g.
#'
#' @param t t test-statistic from a one-sample t-test
#' @param N sample size.
#'
#' @seealso \code{\link{corrJ}} for the function of the correction factor J.
#'
#' @return Hedges' g.
#' @export
hedgeG <- function(t,N){
  G <- (t/sqrt(N))*corrJ(N)
  return(G)
}

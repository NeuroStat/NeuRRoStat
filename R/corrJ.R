#' Correction factor J
#'
#' Helper function for \code{\link{hedgeG}}.
#' Calculates the correction factor in the conversion from a t-statistic
#' to g.
#'
#' @param N sample size in this study
#' @return A positive integer J
#' @export
corrJ <- function(N){
  1-(3/((4*(N-1))-1))
}

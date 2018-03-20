#' Correction factor h
#'
#' Helper function for \code{\link{hedgeG}}.
#' Calculates the exact correction factor for Hedges g
#'
#' @param Ne sample size of the experimental group of subjects in this study
#' If only one sample is used, then use Ne as the sample size in the entire study.
#' @param Nc sample size of the control group of subjects in this study, NULL if type = "one.sample".
#' @param type a character specifying either one of c("one.sample", "two.sample").
#' Default is one.sample as this is usually the case in fMRI.
#' @return An integer h between 0 and 1.
#' @export
corrH <- function(Ne, Nc = NULL, type = c("one.sample",  "two.sample")){
  # Check arguments
  type <- match.arg(type)

  # Check sample sizes: if two values are supplied but type = one.sample, error
  if(type == "one.sample"){
    if(!is.null(Nc)){
      stop('Nc is specified, but type is one.sample')
    }
  }
  # If two.sample, need two sample sizes
  if(type == "two.sample"){
    if(is.null(Nc)){
      stop('Type is two.sample, but Nc is not specified')
    }
  }

  # Compute H
  if(type == "one.sample"){
  num <- gamma((Ne - 1)/2)
  denom <- (sqrt((Ne - 1)/2) * gamma((Ne - 2)/2))
  corr <- num/denom
  }

  if(type == "two.sample"){
    num <- gamma((Ne + Nc - 2)/2)
    denom <- (sqrt((Ne + Nc - 2)/2) * gamma((Ne + Nc - 3)/2))
    corr <- num/denom
  }

  return(corr)
}

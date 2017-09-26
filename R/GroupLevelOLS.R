#' Perform group level model using OLS
#'
#' This function performs simple OLS on the beta 1 parameters obtained fitting a GLM at subject level.
#' The user does not need to specify a design, as this function is only used
#' to calculate one sample tests (corresponding to t-tests).
#'
#' @param N an integer specifying the amount of subjects from the first level
#' @param subj_b1 a vector of length N with the estimated beta 1 parameters
#' @param ... extra arguments passed to or from other methods.
#'
#' @return a vector with the estimated group level parameter
#' @export
GroupLevelOLS <- function(N, subj_b1){
  # Check class of beta 1 parameters
  if(!class(subj_b1) %in% c('vector', 'numeric')) stop("subj_b1 should be a vector.")
  # Check whether length of beta 1 parameters is equal to N
  if(N != length(subj_b1)) stop("Sample size does not match length of beta 1 parameters.")

  # Beta estimate is the average
  allStud_beta1 <- mean(subj_b1, na.rm = TRUE)

  # SE is sqrt(var(Y)/n)
  allStud_se_beta1 <- sqrt(var(subj_b1, na.rm = TRUE)/N)

  # T-value
  allStud_tmap <- allStud_beta1/allStud_se_beta1
  return(list(Beta1 = allStud_beta1, SE = allStud_se_beta1, tvalue = allStud_tmap))
}

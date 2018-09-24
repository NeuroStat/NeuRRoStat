#' Calculate overlap of activation (Maitra's adaptation of Dice).
#'
#' Function to calculate the overlap of activation of two binarized images.
#' The formula is an adaptation of the Dice overlap, suggested by Maitra in 2010.
#'
#' @param bin_map1 the first binarized (thresholded) map
#' @param bin_map2 the second binarized (thresholded) map
#'
#' @return Numeric value: measure of overlap.
#'
#' @details
#' The measure is the number of hits (e.g. voxels) intersecting in both images
#' divided by the sum of the number of hits in each image minus its intersection again:
#' \deqn{V_{a,b}/(V_a + V_b - V_{a,b})}
#' Reference:
#' Maitra, R. (2010). A re-defined and generalized percent-overlap-of- activation measure for studies of fMRI reproducibility and its use in identifying outlier activation maps. Neuroimage 50, 124–135.
#' @export
overlapAct <- function(bin_map1, bin_map2){
  # Check if maps are only binarized
  if(!all(bin_map1 %in% c(0,1))){
    stop('Input should be binary maps only!')
  }
  if(!all(bin_map2 %in% c(0,1))){
    stop('Input should be binary maps only!')
  }

  # Check if both dimensions match
  if(!all(dim(bin_map1) == dim(bin_map2))){
    stop('Dimensions of both maps need to match!')
  }

  # Sum both maps
  SumMap <- bin_map1 + bin_map2

  # Now calculate the number of intersecting voxels when summing
  Vab <- sum(SumMap == 2)
  Va <- sum(bin_map1 == 1)
  Vb <- sum(bin_map2 == 1)

  # Now calculate overlap
  overlap <- Vab/(Va + Vb - Vab)
  return(overlap)
}

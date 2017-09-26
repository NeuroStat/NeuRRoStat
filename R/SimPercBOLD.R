#' Simulate an fMRI time series using a parametric GLM with known beta parameters.
#'
#' This temporarily function creates a time series of nscan for one subject.
#' The user can specify the % change of BOLD signal as beta 1.
#' Then it specifies the range of the design matrix and sigma for white noise.
#' It can be used to run quick simulations.
#'
#' @param percBOLD a number ( in %) providing the true percent change of BOLD change.
#' @param designRange a vector with the min and max of the design.
#' @param nscan a number providing the number of scans
#' @param sigmaNoise a single number representing sigma of white noise
#' @param ... extra arguments passed to or from other methods.
#'
#' @return a list with a vector with the simulated time series, the signal, the design matrix
#' @export
SimPercBOLD <- function(percBOLD, designRange, nscan, sigmaNoise){
  # Test for presence of neuRosim
  if(requireNamespace('neuRosim', quietly = TRUE) == FALSE) stop('Function requires neuRosim')

  # Check whether % change of BOLD signal is between 0 - 100
  if(percBOLD < 0 | percBOLD > 100) stop('% change of BOLD signal should be between 0 and 100')

  # Check whether range of design consists of two numbers
  if(length(designRange) != 2) stop("Please provide min and max of the design matrix")
  designRange <- sort(designRange)
  MaxMinDesign <- max(designRange) - min(designRange)

  # Time series parameters
  TR <- 2
  total.time <- nscan * TR
  dur <- 20
  onsets <- seq(1, total.time, 40)
  base <- 100

  # Generating a design matrix
  X_structure <- simprepTemporal(total.time,1,onsets=onsets, effectsize = MaxMinDesign, durations=dur,
                                 TR = TR, acc=0.1, hrf="double-gamma")
  X <- simTSfmri(design=X_structure, base=base, SNR=1, noise="none", verbose=FALSE)

  # Create a signal based on % BOLD signal changes
  signal <- percBOLD/(MaxMinDesign) * (X-base) + base

  # Add noise
  Y <- signal + rnorm(n = nscan, sd = sigmaNoise)

  return(list(Y = Y, signal = signal, X = X))
}

#' Correction factor J
#'
#' Helper function for \code{\link{hedgeG}}.
#' Calculates the correction factor in the conversion from a t-statistic
#' to g.
#'
#' @param N sample size in this study
#' @return A positive integer J
corrJ <- function(N){
  1-(3/((4*(N-1))-1))
}

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
hedgeG <- function(t,N){
  G <- (t/sqrt(N))*corrJ(N)
  return(G)
}

#' Calculate within-study variance of g
#'
#' Function to calculate the within-study variance of one sample
#' effect size g using calculations from Hedges (1989).
#' Input is the standardized effect size g and N.
#'
#' @param g standardized effect size g from one sample set-up.
#' @param N sample size
#'
#' @return var(g)
varHedge <- function(g,N){
  value <- (1/N) + (1 - (gamma((N - 2) / 2) / gamma((N - 1) / 2))^2 * (N - 3) / 2) * g^2
  return(round(value,7))
}

#' DerSimonian & Laird estimator for between-study variance.
#'
#' Function to estimate \eqn{\tau^2}, the DerSimonian & Laird (1986) estimator
#' for between-study variance. Usefull to execute a random effects meta-analysis.
#'
#' @param Y a vector with the estimates for the study effects
#' (e.g. standardized effect sizes Hedges' g).
#' @param W a vector with the study specific weights.
#' Most often the inverse of the variance of Y.
#' See also \code{\link{varHedge}}.
#' @param k amount of studies in the meta-analysis.
#'
#' @references \url{https://www.ncbi.nlm.nih.gov/pubmed/3802833}
#'
#' @return an estimate for between-study variance.
tau <- function(Y,W,k){
  C <- sum(W)-(sum(W^2)/sum(W))
  df <- k-1
  Q <- sum(W*Y^2)-sum(W*Y)^2/sum(W)
  if(Q < df){
    T2 <- 0
  }else{
    T2 <- (Q-df)/C
  }
  return (T2)
}

#' Calculate a weighted average
#'
#' Function to calculate a weighted average.
#'
#' @param Y the values to calculate the weighted average over.
#' @param W the weights (of length Y or 1) to use in the weighted average
#'
#' @return the weighed average
wMean <- function(Y,W){
  if((length(W) != 1) & (length(W) != length(Y))){
    stop('Vector of weights should be of length Y or 1')
  }
  M <- sum(W*Y)/sum(W)
  return(M)
}






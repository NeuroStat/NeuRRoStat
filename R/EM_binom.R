#' Fit mixture of two binomial distributions using EM.
#'
#' Function to estimate the parameters of a mixture of two binomial distributions
#' using the Expectation-Maximization algorithm (EM).
#' Can be used in a setting where the data contains V independent observations.
#' Each of these is a realization of a binomial data generating process with the
#' probability of success (out of N trials) being either \eqn{\pi_1}{\pi 1} or \eqn{\pi_2}{\pi 2}.
#'
#'
#' @param Y vector of length V with the observed data (number of successes)
#' @param N the amount of trials
#' @param iniL initial value for \eqn{\lambda}
#' @param iniPI1 initial value for \eqn{\pi_1}{\pi 1}
#' @param iniPI2 initial value for \eqn{\pi_2}{\pi 2}
#' @param max.iter maximum iterations (optional)
#' @param tolerance level of tolerance when comparing the likelihood of previous and current step in the algorithm (optional). Convergence is assumed when the absolute difference between both log likelihoods is smaller than the tolerance level.
#'
#' @return Data frame with the parameter estimates and the number of iterations after convergence
#'
#' @details
#' Assume \eqn{Y} is i.i.d. from \deqn{\lambda P_1(Y;k, \pi_1) + (1 - \lambda) P_2(Y;k,\pi_2)}{\lambda P1(Y;k, \pi 1) + (1 - \lambda) P2(Y;k,\pi 2)}
#' The complete log-likelihood of the data over all observations (V) is then given as:
#' \deqn{cst + \sum_{v = 1}^V log(\lambda(\pi 1)^Y(v) (1 - \pi 1)^(N - Y(v)) + (1 - \lambda)(\pi 2)^Y(v) (1 - \pi 2)^(N - Y(v)))}
#' The EM algorithm is then used to estimate \eqn{\lambda}, \eqn{\pi_1}{\pi 1} and \eqn{\pi_2}{\pi 2}.
#'
#' Note on starting values: it is not advised to use the same values for the starting values
#' of \eqn{\pi_1}{\pi 1} and \eqn{\pi_2}{\pi 2} as the algorithm does not seem to handle this
#' setting very well. Starting values far away from the true value do converge to the true value
#' most of the time, but take more iterations.
#' @export
EMbinom <- function(Y, N, iniL, iniPI1, iniPI2, max.iter = 500, tolerance = 0.001){

  # Give warning if iniPI1 and iniPI2 have the same value
  if(iniPI1 == iniPI2) warning('Using the same starting values for both probabilities (pi 1 and pi 2) might lead to bad convergence!')

  ##################
  # Local functions
  ##################

  # Define function weights (using current step in algorithm)
  weight <- function(lambda, pi_1, pi_2, N, Yi){
    # First calculate numinator: lambda times density of binomial
    num <- lambda * dbinom(x = Yi, size = N, prob = pi_1)
    # Now calculate denominator
    denom <- num + ((1 - lambda) * dbinom(x = Yi, size = N, prob = pi_2))
    # Return value
    return(num/denom)
  }

  # Maximalization function that returns updated
  #   value for pi_1, given current values (_c)
  update_pi_1 <- function(lambda_c, pi_1_c, pi_2_c, N, Yi){
    # Weight of current step
    weight_c <- weight(lambda_c, pi_1_c, pi_2_c, N, Yi = Yi)
    # First calculate numinator
    num <- sum(weight_c * Yi)
    # Now calculate denominator
    denom <- sum(weight_c * N)
    # Return value
    return(num/denom)
  }

  # Same for pi_2
  update_pi_2 <- function(lambda_c, pi_1_c, pi_2_c, N, Yi){
    # Weight of current step
    weight_c <- weight(lambda_c, pi_1_c, pi_2_c, N, Yi = Yi)
    # First calculate numinator
    num <- sum(((1 - weight_c) * Yi))
    # Now calculate denominator
    denom <- sum(((1 - weight_c) * N))
    # Return value
    return(num/denom)
  }

  # Same for lambda
  update_lambda <- function(lambda_c, pi_1_c, pi_2_c, N, Yi){
    mean(weight(lambda_c, pi_1_c, pi_2_c, N, Yi))
  }

  ##################
  # EM ALGORITHM  #
  ##################

  # Start for loop of algorithm steps
  for(v in 1:max.iter){
    ################################
    ## 1: Initial starting values ##
    ################################
    if(v == 1){
      lambda <- iniL
      pi_1 <- iniPI1
      pi_2 <- iniPI2
    }

    ################################
    ## 2: Conditional expectation ##
    ################################

    # See local functions

    #####################################
    ## 3: Maximise (update parameters) ##
    #####################################

    # Current log likelihood
    logLik_current <- sum((lambda * pi_1^Y * (1 - pi_1)^(N - Y)) + ((1 - lambda)*pi_2^Y * (1 - pi_2)^(N - Y)))

    # Parameter: pi_A1
    pi_1_up <- update_pi_1(lambda, pi_1, pi_2, N, Y)
    # Parameter: pi_I1
    pi_2_up <- update_pi_2(lambda, pi_1, pi_2, N, Y)
    # Paramater: lambda
    lambda_up <- update_lambda(lambda, pi_1, pi_2, N, Y)

    # Now we replace the parameters
    lambda <- lambda_up
    pi_1 <- pi_1_up
    pi_2 <- pi_2_up

    ###################################
    ## 4: Stopping rule: convergence ##
    ###################################

    # If the absolute difference between the log likelihoods is smaller than the tolerance,
    #  then stop the algorithm
    logLik_update <- sum((lambda * pi_1^Y * (1 - pi_1)^(N - Y)) + ((1 - lambda)*pi_2^Y * (1 - pi_2)^(N - Y)))
    if(abs(logLik_update - logLik_current) < tolerance) break

  }

  # data frame with results
  res <- data.frame('lambda' = lambda, 'PI1' = pi_1, 'PI2' = pi_2, 'num.iter' = v)
  return(res)
}

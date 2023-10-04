#' @title {Simulate the propensity score for the simulated observations}
#'
#' @description Define the probability of receiving the treatment giving a series of simulated baseline covariates
#'
#' @param {int} {Logistic regression model intercept, obtained through `bisection`}
#'
#' @param {alpha} {Logistic regression model coefficients: must have the same length as the number of columns of the matrix obtained through `baseline_covariates`}
#'
#' @param {baseline_covariates} {Simulated baseline covariates obtained throught `baseline_covariates`}
#'
#'

define_p_treat <- function(int, alpha, baseline_covariates) {
  logit_treat <- int + alpha[1] * baseline_covariates[,1] + alpha[2] * baseline_covariates[,2] + alpha[3]*baseline_covariates[,3] + alpha[4] * baseline_covariates[,4] + alpha[5]* baseline_covariates[,5] + alpha[6]*baseline_covariates[,6] + alpha[7] * baseline_covariates[,7] + alpha[8] * baseline_covariates[,8] + alpha[9]*baseline_covariates[,9]
  prob_treat <- exp(logit_treat)/(1 + exp(logit_treat))
  prob_treat
}

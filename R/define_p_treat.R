#' @title {Definizione propensity score osservazioni simulate}
#'
#' @description Definisce la probabilità di ricevere il trattamento per ciascuna delle osservazioni simulate
#'
#' @param {int} {Intercetta del modello di regressione logistica: ottenibile utilizzando la funzione `bisection`}
#'
#' @param {alpha} {Vettore dei coefficienti di regressione logistica tra le variabili confondenti e la probabilità del trattamento, deve avere la stessa lunghezza del numero di colonne della matrice `baseline_covariates`, ovvero 9}
#'
#' @param {baseline_covariates} {Variabili confondenti simulate utilizzando la funzione `baseline_covariates`}
#'
#'

define_p_treat <- function(int, alpha, baseline_covariates) {
  logit_treat <- int + alpha[1] * baseline_covariates[,1] + alpha[2] * baseline_covariates[,2] + alpha[3]*baseline_covariates[,3] + alpha[4] * baseline_covariates[,4] + alpha[5]* baseline_covariates[,5] + alpha[6]*baseline_covariates[,6] + alpha[7] * baseline_covariates[,7] + alpha[8] * baseline_covariates[,8] + alpha[9]*baseline_covariates[,9]
  prob_treat <- exp(logit_treat)/(1 + exp(logit_treat))
  prob_treat
}

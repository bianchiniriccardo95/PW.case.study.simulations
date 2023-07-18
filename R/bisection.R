#' @title {Bisection: definizione dell'intercetta per il modello di stima dei propensity score}
#'
#' @description Per trovare l'intercetta adeguata al modello di stima dei propensity score per ottenere una determinata treatment prevalence desiderata.
#'
#' @param {treatment_prevalence} {La treatment prevalence desiderata (deve andare da 0 a 1)}
#'
#' @param {lower} {Valore minimo che l'intercetta può assumere}
#'
#' @param {upper} {Valore massimo che l'intercetta può assumere}
#'
#' @param {maxIter} {Massimo numero di iterazioni per la ricerca dell'intercetta}
#'
#' @param {maxError} {Errore massimo}
#'
#' @param {baseline_covariates} {Variabili confondenti simulate attraverso la funzione `baseline_covariates`}
#'
#' @param {alpha} {Vettore degli alpha, necessario per la funzione `define_p_treat`}
#'
#'

bisection <- function(treatment_prevalence,lower = -500, upper = 500, maxError = 0.0001, maxIter = 10000, baseline_covariates, alpha){

  assertive::is_numeric(alpha)
  assertive::is_double(treatment_prevalence)
  if(treatment_prevalence > 1){
    stop("La treatment prevalence deve essere un valore compreso tra 0 e 1")
  }

  a <- lower
  b <- upper
  s <- alpha
  iter <- 0
  cov <- baseline_covariates

  treat_a <- define_p_treat(int = a, alpha = s, baseline_covariates = cov)
  treat_b <- define_p_treat(int = b, alpha = s, baseline_covariates = cov)
  c <- (a+b) / 2

  prob_treat_a <- mean(rbinom(nrow(cov), 1, treat_a))
  prob_treat_b <- mean(rbinom(nrow(cov),1, treat_b))

  diff_a <- abs(prob_treat_a - treatment_prevalence)
  diff_b <- abs(prob_treat_b - treatment_prevalence)

  while(iter <= maxIter) {
    c <- (a+b) / 2
    treat_c <- define_p_treat(int = c, alpha = s, baseline_covariates = cov)
    prob_treat_c <- mean(rbinom(nrow(cov),1, treat_c))
    if(abs(prob_treat_c - treatment_prevalence) < maxError) {
      break
    }

    fa <- define_p_treat(int = a, baseline_covariates = cov, alpha = s)
    fb <- define_p_treat(int = b, baseline_covariates = cov, alpha = s)

    prob_treat_a <- mean(rbinom(nrow(cov),1,fa))
    prob_treat_b <- mean(rbinom(nrow(cov),1,fb))

    diff_a <- prob_treat_a - treatment_prevalence
    diff_c <- prob_treat_c - treatment_prevalence

    if(sign(diff_c) == sign(diff_a)) {
      a <- c
    } else {
      b <- c
    }

    iter <- iter+1
  }

  #if(iter >= maxIter) {
  #print(c)
  #stop("Search failed. Increase number of iterations")
  #}

  return(c)

}

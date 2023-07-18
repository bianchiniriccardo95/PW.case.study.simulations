#' @title {Baseline covariates: simulazione delle variabili confondenti}
#'
#' @description Simulazione delle covariate a partire da una distribuzione normale multivariata. In particolare, verranno ad essere simulate delle variabili che dovrebbero simulare le nove covariate confondenti del caso studio: 7 variabili binarie (con una prevalenza identica a quelle del caso studio), e 2 variabili continue (con media e sd pari alle corrispettive del caso studio)
#'
#' @param {n} {Numero di casi che si vuole simulare}
#' @param {mus} {Vettore contenente le medie per ciascuna variabile che si vuole simulare}
#' @param {sds} {Vettore contenente le deviazioni standard}
#' @param {corr_matrix} {Matrice di correlazione tra le variabili che si vuole stimare}
#'
#' @return {Matrice il cui numero di righe è pari al numero di osservazioni che si vuole stimare e il numero di colonne è pari al numero di variabili che si vuole simulare}
#'
#' @export
#'
#'

baseline_covariates <- function(n, mus = c(rep(0,7L),55,26), sds = c(rep(1,7L), 11, 4), corr_matrix) {

  assertive::assert_is_a_number(n)
  assertive::assert_is_numeric(mus)
  assertive::assert_is_numeric(sds)
  assertive::assert_is_symmetric_matrix(corr_matrix)

  cov_matrix <- Matrix::nearPD(corr_matrix)$mat * (sds %*% t(sds))

  mv_norm <- MASS::mvrnorm(n = n, mu = mus, Sigma = cov_matrix)


  mv_norm[, 1] <- ifelse(mv_norm[, 1] > 0, 1, 0) #Gender
  mv_norm[, 2] <- ifelse(mv_norm[, 2] > -0.25, 1, 0) #Education
  mv_norm[, 3] <- ifelse(mv_norm[, 3] > 0.7, 1, 0) #Smoking
  mv_norm[,4] <- ifelse(mv_norm[, 4] > 1.45, 1, 0) #DM
  mv_norm[,5] <- ifelse(mv_norm[, 5] > 1.4, 1, 0) #CVD
  mv_norm[,6] <- ifelse(mv_norm[, 6] > 0.55, 1, 0) #Hypertension
  mv_norm[,7] <- ifelse(mv_norm[, 7] > 1.03, 1, 0) #Albuminuria


  colnames(mv_norm) <- glue::glue("x{1:ncol(mv_norm)}")

  mv_norm

}

#' @title {Baseline covariates: baseline covariates simulation}
#'
#' @description Baseline covariates simulation, starting from a multivariate normal distribution. Particularly, the simulated variables would resemble the case study ones: 7 binary variables (with the same prevalence as in the case study), and 2 continuous variables (with the same mean and sd as in the case study)
#'
#' @param {n} {Number of simulated cases}
#' @param {mus} {Vector of the means for each simulated variable}
#' @param {sds} {Vector of the sds for each simulated variable}
#' @param {corr_matrix} {Matrix of correlations between simulated variables}
#'
#' @return {A matrix in which each row corresponds to a simulated case and each column corresponds to a simulated variable}
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

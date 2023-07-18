#' @title {Simulation results}
#'
#' @description Estrarre i risultati delle simulazioni in termini di bias, SMD e RMSE
#'
#' @param {sim_results} {Output della funzione `simulation`}
#' @param {treatment_effect} {Treatment effect stabilito, deve essere uguale a quello di simulation, default -0.2}
#' @export results Una lista contenente gli ate estimates, il bias, il rmse e l'average SMD
#'
#'
simulation_results <- function(sim_results, treatment_effect = -0.2){
  smd_df <- data.frame()
  for (i in 1:length(sim_results)){
    smd_data <- sim_results[[i]][[1]]
    smd_df <- rbind(smd_df, smd_data)
  }
  averaged_smd <- mean(colMeans(smd_df))

  ate_estimates <- map_dbl(sim_results,
                           ~ {
                             ate_results <- .x$results_ate
                             ate_results$coefficients[2]
                           })
  bias <- mean(ate_estimates) - treatment_effect
  rmse <- sqrt((sd(ate_estimates)^2) + bias^2)
  results <- list(
    average_smd = averaged_smd,
    ate_estimates = ate_estimates,
    bias = bias,
    rmse = rmse
  )
  results
}

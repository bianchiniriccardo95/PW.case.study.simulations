#' @title {Simulation results}
#'
#' @description Extract the simulation results in terms of Absolute Standardized Mean Difference, Bias and RMSE
#'
#' @param {sim_results} {Output of `simulation`}
#' @param {treatment_effect} {Desired treatment effect, default -0.9}
#' @export results A list containing the ATE estimates, average ASMD,bias and RMSE
#'
#'
simulation_results <- function(sim_results, treatment_effect = -0.9){
  smd_df <- data.frame()
  for (i in 1:length(sim_results)){
    smd_data <- sim_results[[i]][[1]]
    smd_df <- rbind(smd_df, smd_data)
  }
  averaged_smd <- mean(colMeans(smd_df, na.rm = T))

  ate_estimates <- map_dbl(filter(function(a) any(!is.na(a)), sim_results),
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

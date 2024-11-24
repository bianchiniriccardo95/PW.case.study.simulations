#' @title {Simulation: Monte Carlo simulation}
#'
#' @description Simulated a specified configuration
#'
#' @param {n} {Number of simulated observations}
#' @param {n_datasets} {Number of simulated dataset, default 1000}
#' @param {seed} {Random seed, default 42}
#' @param {treatment_prevalence} {Desired treatment prevalence}
#' @param {treatment_effect} {Fixed treatment effect, default -0.9}
#' @param {approach} {Type of approach, i.e. crude, IPW, OW, EBAL}
#' @param {trim} {If IPW, for specifying the desired trimming cutoff (1 means no trimming)}
#'
#' @return {A list containing: balancing levels, bias and RMSE}


simulation <- function(n, n_datasets = 1000, seed = 42, treatment_prevalence, treatment_effect = -0.2, approach = c('crude', 'IPW', 'OW', 'EBAL'), trim = NULL){
  set.seed(seed)
  corr_matrix <- corr_matrix_statin(statin_db_corr)
  alpha <- define_alpha(statin_db)
  b <- define_beta(statin_db)
  simulation_results <- map(
    1:n_datasets,
    ~ {
      baseline_covariates <- baseline_covariates(n = n, corr_matrix = corr_matrix)
      intercept <- bisection(treatment_prevalence = treatment_prevalence, baseline_covariates = baseline_covariates,alpha = alpha)
      prob_treat <- define_p_treat(int = intercept, alpha = alpha, baseline_covariates = baseline_covariates)
      treats <- rbinom(n = n, size = 1, prob = prob_treat)
      simulated_dataset <- as.data.frame(baseline_covariates) %>%
        mutate(treatment = factor(treats))
      y_0 <- rnorm(nrow(simulated_dataset), (b[1] + treatment_effect * 0 + b[2] * baseline_covariates[,1] + b[3] * baseline_covariates[,2] +b[4] * baseline_covariates[,3] +b[5] * baseline_covariates[,4] +b[6] * baseline_covariates[,5] +b[7] * baseline_covariates[,6] +b[8] * baseline_covariates[,7] +b[9] * baseline_covariates[,8] +b[10] * baseline_covariates[,9] ), 1)
      y_1 <- rnorm(nrow(simulated_dataset), (b[1] + treatment_effect * 1 + b[2] * baseline_covariates[,1] + b[3] * baseline_covariates[,2] +b[4] * baseline_covariates[,3] +b[5] * baseline_covariates[,4] +b[6] * baseline_covariates[,5] +b[7] * baseline_covariates[,6] +b[8] * baseline_covariates[,7] +b[9] * baseline_covariates[,8] +b[10] * baseline_covariates[,9] ), 1)
      simulated_dataset <- simulated_dataset %>%
        mutate(y0 = y_0, y1 = y_1) %>%
        mutate(y_observed = ifelse(treatment == 0, y0, y1))

      if (approach == 'crude') {
        bal_tab <- bal.tab(weightit(treatment ~ x1+x2+x3+x4+x5+x6+x7+x8+x9, data = simulated_dataset), un = T, abs = TRUE, stats = c('mean.diffs','variance.ratios'))
        unbalanced_smd <- bal_tab$Balance[2:nrow(bal_tab$Balance), 'Diff.Un']
        results_ate <- summary(lm(y_observed ~ treatment, data = simulated_dataset))
        output <- list(
          smd_results = unbalanced_smd,
          results_ate = results_ate
        )
      }

      else if ((approach == 'IPW') & (is.null(trim))){
        weightit_ipw <- weightit(treatment ~ x1 + x2 +x3 +x4+x5+x6+x7+x8+x9, data = simulated_dataset, estimand = 'ATE', method = 'ps')
        simulated_dataset_wt <- simulated_dataset %>% mutate(wts = weightit_ipw$weights)
        svy <- svydesign(ids = ~ 1, weights = ~ wts, data = simulated_dataset_wt)
        bal_tab <- bal.tab(x = simulated_dataset_wt[,c(1:9)], weights = simulated_dataset_wt$wts, s.d.denom = 'pooled', treat = simulated_dataset_wt$treatment, un = T, abs = TRUE, stats = c('mean.diffs','variance.ratios'))
        ipw_balanced_smd <- bal_tab$Balance[2:nrow(bal_tab$Balance), 'Diff.Adj']
        output <- list(
          smd_results = ipw_balanced_smd, #smd delle diverse covariate in ogni dataset
          results_ate = summary(svyglm(y_observed ~ treatment, design = svy, family = gaussian()))
        )
      }

      else if ((approach == 'IPW') & (!is.null(trim))){
        weightit_ipw <- weightit(treatment ~ x1 + x2 +x3 +x4+x5+x6+x7+x8+x9, data = simulated_dataset, estimand = 'ATE', method = 'ps')
        simulated_dataset_wt <- simulated_dataset %>% mutate(wts = weightit_ipw$weights)
        simulated_dataset_wt <- simulated_dataset %>% mutate(wts_trimmed = trim(simulated_dataset_wt$wts, at = trim, treat = simulated_dataset_wt$treatment))
        svy <- svydesign(ids = ~ 1, weights = ~ wts_trimmed, data = simulated_dataset_wt)
        bal_tab <- bal.tab(x = simulated_dataset_wt[,c(1:9)], weights = simulated_dataset_wt$wts_trimmed, s.d.denom = 'pooled', treat = simulated_dataset_wt$treatment, un = T, abs = TRUE, stats = c('mean.diffs','variance.ratios'))
        ipw_balanced_smd <- bal_tab$Balance[2:nrow(bal_tab$Balance), 'Diff.Adj']
        output <- list(
          smd_results = ipw_balanced_smd, #smd delle diverse covariate in ogni dataset
          results_ate = summary(svyglm(y_observed ~ treatment, design = svy, family = gaussian()))
        )
      }

      else if (approach == 'OW'){
        weightit_ow <- weightit(treatment ~ x1 + x2 +x3 +x4+x5+x6+x7+x8+x9, data = simulated_dataset, estimand = 'ATO', method = 'ps')
        bal_tab <- bal.tab(weightit_ow, un = T, abs = TRUE, stats = c('mean.diffs','variance.ratios'))
        ow_balanced_smd <- bal_tab$Balance[2:nrow(bal_tab$Balance), 'Diff.Adj']
        simulated_dataset_wt <- simulated_dataset %>% mutate(wts = weightit_ow$weights)
        svy <- svydesign(ids = ~ 1, weights = ~ wts, data = simulated_dataset_wt)
        output <- list(
          smd_results = ow_balanced_smd, #smd delle diverse covariate in ogni dataset
          results_ate = summary(svyglm(y_observed ~ treatment, design = svy, family = gaussian()))
        )
      }

      else if (approach == 'EBAL'){
        weightit_ebal <- weightit(treatment ~ x1 + x2 +x3 + x4 + x5 + x6 + x7 + x8 + x9, data = simulated_dataset, estimand = 'ATE', method = 'ebal')
        bal_tab <- try(bal.tab(weightit_ebal, un = T, abs = TRUE, stats = c('mean.diffs','variance.ratios')))
        if (inherits(bal_tab, 'try-error')){
          output <- list(
            smd_results = NA,
            results_ate = NA
          )
        } else{
          ebal_balanced_smd <- bal_tab$Balance[2:nrow(bal_tab$Balance), 'Diff.Adj']
          simulated_dataset_wt <- simulated_dataset %>% mutate(wts = weightit_ebal$weights)
          svy <- svydesign(ids = ~ 1, weights = ~ wts, data = simulated_dataset_wt)
          output <- list(
            smd_results = ebal_balanced_smd, #smd delle diverse covariate in ogni dataset
            results_ate = summary(svyglm(y_observed ~ treatment, design = svy, family = gaussian()))
          )
        }
      }

      output
    }, .progress = TRUE
  )
  simulation_results
}

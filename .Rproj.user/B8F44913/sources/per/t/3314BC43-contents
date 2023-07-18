#' @title {Define alpha: definire i coefficienti di regressione logistica per le variabili confondenti}
#'
#' @param {data} {Base di dati iniziali, che sarebbe quella del mio caso studio}
#'
#'

define_alpha <- function(data){
  coefficients <- glm(Statin ~ Gender + Education + Smoking + DM + CVD + Hypertension + Albuminuria_1 + Age + BMI,
                      data = data,
                      family = binomial("logit"))$coefficients[2:10]
  coefficients
}

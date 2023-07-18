#' @title {Define beta: definire i coefficienti di regressione per la simulazione della variabile di outcome}
#'
#' @param {data} {Base di dati, ovvero quella del nostro caso studio}
#'
#'
define_beta <- function(data){
  coefficients <- c(mean(data$RFFT), summary(lm(RFFT ~ Gender + Education + Smoking + DM + CVD + Hypertension + Albuminuria_1 + Age + BMI, data = data))$coefficients[2:10])
  coefficients
}

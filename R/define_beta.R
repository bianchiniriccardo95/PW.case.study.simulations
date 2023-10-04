#' @title {Define beta: define the coefficients for simulating the outcome}
#'
#' @param {data} {Case study dataset}
#'
#'
define_beta <- function(data){
  coefficients <- c(mean(data$RFFT), summary(lm(RFFT ~ Gender + Education + Smoking + DM + CVD + Hypertension + Albuminuria_1 + Age + BMI, data = data))$coefficients[2:10])
  coefficients
}

#' @title {Define alpha: define the logistic regression coefficients}
#'
#' @param {data} {Case study dataset}
#'
#'

define_alpha <- function(data){
  coefficients <- glm(Statin ~ Gender + Education + Smoking + DM + CVD + Hypertension + Albuminuria_1 + Age + BMI,
                      data = data,
                      family = binomial("logit"))$coefficients[2:10]
  coefficients
}

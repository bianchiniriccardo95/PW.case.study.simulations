#' @title {corr_matrix_statin: estrazione della matrice di correlazione dal caso studio}
#'
#' @param {data} {Dataset del caso studio sull'utilizzo di statine}
#'
#'

corr_matrix_statin <- function(data){
  categorical_variables <- c('Gender', 'Education', 'Smoking' ,'DM', 'CVD', 'Hypertension', 'Albuminuria_1')
  statin_db_sim_data <- data %>%
    mutate(Education = ifelse(Education == 2 | Education == 3, 1, 0),
           Smoking = ifelse(Smoking == -1, 0, Smoking))
  corr_matrix <- round(cor(as.matrix((statin_db_sim_data[,c(categorical_variables,'Age', 'BMI')]))),1)
  corr_matrix
}

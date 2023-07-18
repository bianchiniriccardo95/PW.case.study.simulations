## code to prepare `statin_db` dataset goes here
library(readxl)
library(tidyverse)
statin_db <- readxl::read_excel("~/Desktop/Università/Master Padova/Biostatistica Avanzata per la Ricerca Clinica/Project Work/statin_use_bis.xlsx")
statin_db <- statin_db %>%
  mutate(Education = factor(ifelse(Education == 2 | Education == 3, 1, 0)),
         Smoking = factor(ifelse(Smoking == -1, 0, Smoking)))

categorical_variables <- c('Gender', 'Ethnicity', 'Education', 'CVD', 'DM', 'Smoking', 'Hypertension', 'Albuminuria_1', 'Albuminuria_2', 'Statin')

statin_db <- statin_db %>% mutate_at(categorical_variables, factor)
usethis::use_data(statin_db, overwrite = TRUE)

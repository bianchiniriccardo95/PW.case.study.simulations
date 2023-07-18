library(readxl)
library(tidyverse)
statin_db_corr <- readxl::read_excel("~/Desktop/Università/Master Padova/Biostatistica Avanzata per la Ricerca Clinica/Project Work/statin_use_bis.xlsx")

usethis::use_data(statin_db_corr, overwrite = TRUE)

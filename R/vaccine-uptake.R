library(readxl)
source("R/config.R")

# Only load vaccine data if it's not already loaded
if(!exists("vaccine_data")) {
  vaccine_data <- read_excel(
    path = paste0(
      data_path, 
      "/covid_vaccinations-oct23_to_sept24.xlsx"
    )
  )
}
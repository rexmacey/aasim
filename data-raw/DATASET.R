## code to prepare `DATASET` dataset goes here
library(readxl)
sbi <- read_excel("data-raw/SBI.xlsx", col_types = c("date",
"numeric", "numeric", "numeric"))
sbi$Month <- as.Date(sbi$Month)
mortality <- readRDS("data-raw/mortality.rds")
usethis::use_data(mortality, sbi, overwrite = TRUE, internal = TRUE)

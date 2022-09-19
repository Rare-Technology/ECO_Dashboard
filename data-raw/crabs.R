library(readxl)

crab.surveys <- readxl::read_excel("../data/ECO/brazil-crabs.xlsx")

usethis::use_data(crab.surveys, overwrite = TRUE)

## code to prepare `seagrass` dataset goes here

# Mozambique seagrass data
# From the Seagrass Surveys project from @raremozambique
# Seagrass_Memba_Ihla_de_Mozambique_Dec2020_clean.csv
# https://data.world/raremozambique/seagrass-surveys/
# there is some character or characters that is not parsed by read.csv, but readr's
# read_csv works
df <- readr::read_csv("https://query.data.world/s/5o6gw5svmfo7eezkmozxjqlrgwcrge")

df$year <- 2020

seagrass.surveys <- df

usethis::use_data(seagrass.surveys, overwrite = TRUE)

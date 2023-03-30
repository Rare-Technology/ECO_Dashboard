library(data.world)

seagrass.surveys <- data.world::query(
  data.world::qry_sql("SELECT * FROM seagrass_memba_ihla_de_mozambique_dec2020_clean"),
  "https://data.world/raremozambique/seagrass-surveys"
)

seagrass.surveys <- seagrass.surveys %>% 
  dplyr::mutate(
    year = 2020,
    ma_name = dplyr::recode(
      ma_name,
      "Memba" = "Memba-sede"
    )
  ) %>% 
  dplyr::rename(location_name = survey_location)

usethis::use_data(seagrass.surveys, overwrite = TRUE)

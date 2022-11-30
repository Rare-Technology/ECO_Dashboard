benthic.surveys <- readr::read_csv("https://query.data.world/s/cstyeadgyj2fcyv5reb3pko23xdhvh")

benthic.surveys <- benthic.surveys %>% 
  dplyr::select(
    -sitename,
    -controlsite,
    -controlsitename,
    -surveytime,
    -divername,
    -temp_c,
    -avg_complexity,
    -methodology
  )

benthic.surveys <- benthic.surveys %>% 
  dplyr::mutate(location_status = dplyr::recode(location_status,
    .missing = "Unknown"
  ))


usethis::use_data(benthic.surveys, overwrite = TRUE)

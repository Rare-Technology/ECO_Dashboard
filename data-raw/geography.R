geo <- fish.surveys %>%
  dplyr::distinct(country, level1_name, level2_name, ma_name) %>% 
  dplyr::union(
    benthic.surveys %>% 
      dplyr::distinct(country, level1_name, level2_name, ma_name)
  ) %>% 
  dplyr::union(
    mangrove.surveys %>% 
      dplyr::distinct(country, level1_name, level2_name, ma_name)
  ) %>% 
  dplyr::union(
    seagrass.surveys %>% 
      dplyr::distinct(country, level1_name, level2_name, ma_name)
  ) %>% 
  dplyr::union(
    crab.surveys %>% 
      dplyr::distinct(country, level1_name, level2_name, ma_name)
  ) %>% 
  dplyr::union(
    oyster.surveys %>% 
      dplyr::distinct(country, level1_name, level2_name, ma_name)
  )

usethis::use_data(geo, overwrite = TRUE)
library(readxl)

crab.surveys <- readxl::read_excel("../data/ECO/brazil-crabs.xlsx") %>% 
  dplyr::mutate(
    sampling_year = ifelse(sampling_year == 2019, 2019, 2020),
    level1_name = ma_name,
    level2_name = ma_name,
    crab_density_ind_ha = burrow_no_Total / quadrat_area_m2 * 10000,
    sampling_location_status = "Managed access" #TBD !!!
    ) %>% 
  dplyr::rename(
    location_status = sampling_location_status,
    year = sampling_year,
    location_name = sampling_location_id
  )

usethis::use_data(crab.surveys, overwrite = TRUE)

library(readxl)
library(dplyr)

oyster.surveys <- readxl::read_excel("../data/ECO/brazil-oysters.xlsx") %>% 
  dplyr::filter(species == "Crassostrea gasar") %>% 
  dplyr::mutate(
    oyster_density_ind_ha = count_alive / quadrat_area_m2 * 10000,
    level2_name = ma_name,
    level1_name = ma_name
  ) %>% 
  dplyr::rename(
    location_status = sampling_location_status,
    year = sampling_year,
    location_name = sampling_location_id
  )

usethis::use_data(oyster.surveys, overwrite = TRUE)

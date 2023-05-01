library(readxl)
library(dplyr)

oyster.surveys <- readxl::read_excel("../data/ECO/brazil-oysters.xlsx") %>% 
  dplyr::filter(species == "Crassostrea gasar") %>% 
  dplyr::mutate(
    oyster_density_ind_ha = count_alive / quadrat_area_m2 * 10000,
    country = "Brazil",
    level1_name = "Pará",
    level2_name = "Curuçá",
    ma_name = "RESEX Mãe Grande de Curuça",
    # Some sampling_day values are saved as a numeric epoch; the number of days since January 1 1900
    # So we will use ifelse to find these dates and convert them to a dd/mm/yyyy string
    sampling_day = ifelse(is.numeric(sampling_day),
      format(as.POSIXct(sampling_day, origin = "1900-01-01", tz = "GMT"), "%d/%m/%Y"),
      sampling_day
    ),
    sampling_time = format(sampling_time, "%H:%M")
  ) %>% 
  dplyr::rename(
    location_status = sampling_location_status,
    year = sampling_year,
    location_name = sampling_location_id
  )

usethis::use_data(oyster.surveys, overwrite = TRUE)

## code to prepare `geo_levels` dataset goes here

geo_levels <- rbind(geo_levels,
  data.frame(country = c("Honduras", "Honduras", "Honduras"),
             level1_name = c("Cortés", "Colón", "Islas de la Bahía"),
             level2_name = c("Puerto Cortés", "Trujillo", "Roatán"),
             ma_name = c("Puerto Cortés", "Trujillo", "Roatán")
  )
)

usethis::use_data(geo_levels, overwrite = TRUE)

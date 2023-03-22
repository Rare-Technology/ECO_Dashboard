library(data.world)

fish.surveys <- data.world::query(
  data.world::qry_sql("SELECT * FROM fish_surveys_all"),
  "https://data.world/rare/fish-surveys"
)

usethis::use_data(fish.surveys, overwrite = TRUE)

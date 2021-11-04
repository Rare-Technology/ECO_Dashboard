## code to prepare `fish.surveys` dataset goes here
##
## fish.surveys came with the old version of the ECO dashboard, it had data for surveys before 2021.

## Cleaning original dataset
fish.surveys <- fish.surveys %>% 
  mutate(country = as.character(country)) %>% 
  mutate(level1_name = as.character(level1_name)) %>% 
  mutate(level2_name = as.character(level2_name)) %>% 
  mutate(ma_name = as.character(ma_name)) %>% 
  mutate(location_status = as.character(location_status)) %>% 
  mutate(species = as.character(species)) %>% 
  mutate(family = as.character(family)) %>% 
  mutate(size_class = as.character(size_class)) %>% 
  mutate(survey_date = lubridate::ymd(survey_date))
  

## 2021 Indonesia Fish Surveys
## From the fish surveys dataset https://data.world/rare/fish-surveys/workspace/file?filename=se_sulawesi_monitoring_biofisik_2021-beltfish-obs-20210809.csv
df <- read.csv("https://query.data.world/s/ujuauezgztamzbxqyl5olmbugpw7s2", header=TRUE, stringsAsFactors=FALSE);
df <- df %>% select(
  country = Country,
  year = Year,
  month = Month,
  day = Day,
  lat = Latitude,
  lon = Longitude,
  location_name = Site,
  location_status = Management.name,
  transect_no = Transect.number,
  transect_length = Transect.length.surveyed,
  count = Count,
  family = Fish.family,
  species = Fish.taxon,
  biomass_kg_ha = Biomass_kgha,
  length = Size,
  a,
  b
)

get_size_class <- function (x) { # need to fix this for existing fish data, its a mess
  if (x <= 5) {"0-5"}
  else if (x > 5 & x <= 10) {"5-10"}
  else if (x > 10 & x <= 20) {"10-20"}
  else if (x > 20 & x <= 30) {"20-30"}
  else if (x > 30 & x <= 40) {"30-40"}
  else if (x > 40 & x <= 50) {"40-50"}
}
get_size_class <- Vectorize(get_size_class)

get_ma_name <- function (x) {
  switch(substr(x, 1, nchar(x)-1),
    "KAPUN" = "Kapuntori",
    "KULIS" = "Kulisusu",
    "MAGIN" = "Maginti",
    "MATA0" = "Mataoleo",
    "MAWAS" = "Mawasangka",
    "MORAM" = "Teluk Moramo",
    # "NTZ" = "",
    "PASIK" = "Pasi Kolaga",
    "SAGOR" = "Sagori",
    "SIOMP" = "Siompu",
    "SIONT" = "Siotapina - Lasel",
    "TALRAY" = "Talaga Raya",
    "TIWOR" = "Tiworo Utara",
    # "UZ" = "",
    "WABUL"  = "Wabula",
    "WAWON" = "Wawonii")
}
get_ma_name <- Vectorize(get_ma_name)

get_level_name <- function (x, level) {
  if (x %in% unique(fish.surveys$ma_name)) {
    fish.surveys %>% 
      filter(ma_name == x) %>% 
      pull(level) %>% 
      unique()
  } else {
    NA
  }
}
get_level_name <- Vectorize(get_level_name)

df <- df %>% 
  mutate(country = "IDN") %>% 
  mutate(survey_date = lubridate::ymd(paste(year, month, day, sep = "-"))) %>% 
  mutate(transect_width = ifelse(length < 35, 5, 20)) %>% # based on surveying methodology
  mutate(transect_area = transect_width * transect_length) %>% 
  mutate(weight = a * length ^ b / 1000) %>% 
  mutate(density_ind_ha = biomass_kg_ha / weight) %>% 
  mutate(size_class = as.character(get_size_class(length))) %>% 
  mutate(ma_name = as.character(get_ma_name(location_name))) %>% 
  mutate(level1_name = get_level_name(ma_name, "level1_name")) %>%
  mutate(level2_name = get_level_name(ma_name, "level2_name"))

## Combine 2021 IDN data with 2019 records

fish.surveys <- fish.surveys %>% select(intersect(names(df), names(fish.surveys)))
df <- df %>% select(names(fish.surveys))
fish.surveys <- rbind(fish.surveys, df)
fish.surveys <- fish.surveys %>% 
  mutate(year = lubridate::year(survey_date))

# MOZ 2020 survey was at the end of the year and about 50 records have a survey date that
# went into January. We will label the year on these as being 2020 as they belong to the
# 2020 survey
fish.surveys$year[fish.surveys$country == "MOZ" & fish.surveys$year == 2021] <- 2020

usethis::use_data(fish.surveys, overwrite = TRUE)

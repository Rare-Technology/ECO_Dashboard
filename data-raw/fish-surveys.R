## code to prepare `fish.surveys` dataset goes here
##
## fish_surveys_all from Dive Surveys project https://data.world/rare/dive-surveys/
fish.surveys <- readr::read_csv("https://query.data.world/s/vigzulfjfm7fh7mah2q3tctcx76exu");

# Removing columns that have a lot of missing information/have no use in the dashboard.
fish.surveys <- fish.surveys %>% 
  dplyr::select(-c(submittedon, submittedby, control_site, control_site_name, diver_name, temp))

# Add subnational and local info
get_level_name <- function (x, level) {
  if (x %in% unique(geo_levels$ma_name)) {
    geo_levels %>% 
      dplyr::filter(ma_name == x) %>% 
      dplyr::pull(level) %>% 
      unique()
  } else if (x == "Teluk Kolono") {
    switch(level,
           "level1_name" = "Southeast Sulawesi",
           "level2_name" = "Konawe Selatan"
    )
  } else {
    NA
  }
}
get_level_name <- Vectorize(get_level_name)

# Fix some ma_name spellings to ensure they'll pass through get_level_names correctly
fish.surveys$ma_name[fish.surveys$ma_name == "Santa Fe"] <- "Santa Fé"
fish.surveys$ma_name[fish.surveys$ma_name == "Puerto Cortes"] <- "Puerto Cortés"
fish.surveys$ma_name[fish.surveys$ma_name == "Roatan"] <- "Roatán"

fish.surveys <- fish.surveys %>% 
  dplyr::mutate(
    level1_name = get_level_name(ma_name, 'level1_name'),
    level2_name = get_level_name(ma_name, 'level2_name')
  )

# to get the year, for almost all records, the following does work:
fish.surveys$year <- lubridate::year(fish.surveys$survey_date)

# But MOZ 2020 survey was at the end of the year and about 50 records (out of 504) have a survey date that
# went into January. We will label the year on these as being 2020 as they belong to the
# 2020 survey
fish.surveys$year[fish.surveys$country == "MOZ" & fish.surveys$year == 2021] <- 2020

#### New IDN dataset
# Raymond provided an updated dataset that has the 2021 survey as well has historical records for 2019 and 2017
# This dataset includes the community site name for the survey sites; NTZ and UTZ correspond to Teluk Kolono
# Teluk Kolono in turn belongs to the LGU Konawe Selatan, which then belongs to SNU South East Sulawesi
# since the weight-length formula had to be used in fish.surveys
# For this stage of data cleaning, we will:
# 1. Select appropriate columns from the updated dataset
# 2. Add level1_name/level2_name columns
# 3. Drop the 2019 IDN records in fish.surveys
# 4. Merge the new 2017/19/21 records with fish.surveys

get_size_class <- function (x) { # need to fix this for existing fish data, its a mess
  if (x <= 5) {"0-5"}
  else if (x > 5 & x <= 10) {"5-10"}
  else if (x > 10 & x <= 20) {"10-20"}
  else if (x > 20 & x <= 30) {"20-30"}
  else if (x > 30 & x <= 40) {"30-40"}
  else if (x > 40 & x <= 50) {"40-50"}
}
get_size_class <- Vectorize(get_size_class)

df <- readr::read_csv("../data/IDN_fish_data.csv")
df <- df %>%
  # 1
  dplyr::select(
    country = Country,
    year = Year,
    month = Month,
    day = Day,
    lat = Latitude,
    lon = Longitude,
    ma_name = `MAR Name`,
    location_name = Site,
    location_status = `Management name`,
    transect_no = `Transect number`,
    count = Count,
    family = `Fish family`,
    species = `Fish taxon`,
    density_ind_ha = density_countha,
    biomass_kg_ha = Biomass_kgha,
    length = Size,
    a,
    b,
    reef_slope = `Reef slope`,
    reef_zone = `Reef zone`,
  ) %>% 
  # 2
  dplyr::mutate(
    country = "IDN",
    survey_date = lubridate::ymd(paste(year, month, day, sep = "-")),
    size_class = as.character(get_size_class(length)),
    level1_name = get_level_name(ma_name, "level1_name"),
    level2_name = get_level_name(ma_name, "level2_name")
  )

# drop lmax from fish.surveys, drop 2019 Indonesia records
fish.surveys <- fish.surveys %>%
  dplyr::select(intersect(names(df), names(fish.surveys))) %>% 
  dplyr::filter(country != "IDN" | year == 2018)

# drop month, day from df
df <- df %>% dplyr::select(names(fish.surveys))

# 4
fish.surveys <- rbind(fish.surveys, df)

##### PHL fish surveys, 2011-2021 #####
### From Rare Fish Surveys: PHL_Master_Fish_2012_2021_Final.csv (think the 2012 part is a typo)
### https://data.world/rare/fish-surveys/
df <- readr::read_csv("https://query.data.world/s/4fsbqm5yxnrtz2n26ukkagl6kthpgm");

# 186,764 records
df <- df %>% dplyr::select(
  country = Country,
  year = Year,
  lat = Latitude, # along with lon, 13,881 are NA
  lon = Longitude,
  ma_name = Management.secondary.name,
  location_name = Site,
  location_status = Management.name,
  transect_no = Transect.number,
  count = Count,
  family = Fish.family,
  species = Fish.taxon,
  biomass_kg_ha = Biomass_kgha,
  length = Size,
  a = a,
  b = b,
  reef_slope = Reef.slope,
  reef_zone = Reef.zone
)

### Columns missing:
# - density_ind_ha (to be calculated)
# - survey_date (insufficient data to process; that are Year, Month, Day columns but
#   only about 15k records out of 127k have Month and Day recorded
# - size_class (to be calculated but not important)
# - level1_name (could try to process using existing geographic level info, but not sure if
#   the info we have is historically accurate)


### Number density
df <- df %>%
  dplyr::mutate(
    weight_kg = a * length^b / 1000,
    density_ind_ha = biomass_kg_ha / weight_kg
  ) %>% 
  dplyr::select(
    -weight_kg
  )


### survey date
df$survey_date <- NA

### Size class
get_size_class <- function (x) { # need to fix this for existing fish data, its a mess
  if (is.na(x)) {NA}
  else if (x <= 5) {"0-5"}
  else if (x > 5 & x <= 10) {"5-10"}
  else if (x > 10 & x <= 20) {"10-20"}
  else if (x > 20 & x <= 30) {"20-30"}
  else if (x > 30 & x <= 40) {"30-40"}
  else if (x > 40 & x <= 50) {"40-50"}
  else if (x > 50) {"50+"}
}
get_size_class <- Vectorize(get_size_class)

df$size_class <- as.character(get_size_class(df$length))

### level1/level2_name
# The dataset is missing snu/lgu names. The ma_name is in the form
# snu_ma. For example, Negros_Oriental_Ayungon is the ma Ayungon from snu
# Negros Oriental.
# So, we can rewrite some of these ma_name's like
# Negros Oriental_Ayungon
# Then use tidyr::separate on "_" to create the level1_name col and a new ma_name col
# We will have to manually edit a few (like Sta. Monica -> Santa Monica)
# The lgu seems to always match the ma name (according to footprint data), so
# we will copy the new ma_name column and call it level2_name

# footprint_global from https://data.world/rare/footprint/
phl_footprint <- readr::read_csv("https://query.data.world/s/6k6dp5zatacjashdxo2v644qsxwnnf") %>% 
  dplyr::filter(country == "Philippines") %>% 
  dplyr::distinct(country, ma_name, level1_name, level2_name)

df <- df %>% 
  dplyr::rename(snu_maa = ma_name) %>% 
  dplyr::mutate(
    snu_maa = dplyr::recode(snu_maa,
      "Camarines_Norte_Mercedes" = "Camarines Norte_Mercedes",
      "Camarines_Sur_Sagnay" = "Camarines Sur_Sagnay",
      "Camarines_Sur_Tinambac" = "Camarines Sur_Tinambac",
      "Cebu_San_Francisco" = "Cebu_San Francisco",
      "Negros Occidental_San_Carlos" = "Negros Occidental_San Carlos",
      "Negros Occidental_Tayasan" = "Negros Oriental_Tayasan", # original data had typo
      "Negros_Oriental_Ayungon" = "Negros Oriental_Ayungon",
      "Negros_Oriental_Bindoy" = "Negros Oriental_Bindoy",
      "Negros_Oriental_Manjuyod" = "Negros Oriental_Manjuyod",
      "Occidental_Mindoro_Looc" = "Occidental Mindoro_Looc",
      "Occidental_Mindoro_Lubang" = "Occidental Mindoro_Lubang",
      "Surigao_del_Norte_Burgos" = "Surigao Del Norte_Burgos",
      "Surigao_del_Norte_Del_Carmen" = "Surigao Del Norte_Del Carmen",
      "Surigao_del_Norte_Dapa" = "Surigao Del Norte_Dapa",
      "Surigao_del_Norte_General_Luna" = "Surigao Del Norte_General Luna",
      "Surigao_del_Norte_Pilar" = "Surigao Del Norte_Pilar",
      "Surigao_del_Norte_San_Benito" = "Surigao Del Norte_San Benito",
      "Surigao_del_Norte_San_Isidro" = "Surigao Del Norte_San Isidro",
      "Surigao_del_Norte_Socorro" = "Surigao Del Norte_Socorro",
      "Surigao_del_Norte_Sta. Monica" = "Surigao Del Norte_Santa Monica",
      "Surigao_Del_Sur_Cantilan" = "Surigao Del Sur_Cantilan",
      "Surigao_Del_Sur_Cortes" = "Surigao Del Sur_Cortes",
      "Zamboanga _Ibugay_Ipil" = "Zamboanga Sibugay_Ipil"
    )
  ) %>% 
  tidyr::separate(snu_maa, c("level1_name", "ma_name"), "_") %>% 
  dplyr::left_join(phl_footprint, by=c("country", "level1_name", "ma_name")) %>% 
  # A couple lgu names did not match with anything on the footprint table
  # Since nearly all the other lgu names match the ma name, we'll just fill in
  # the missing lgu names to match their ma name.
  dplyr::mutate(
    level2_name = dplyr::recode(level2_name,
      .missing = ma_name
    )
  )

fish.surveys <- fish.surveys %>% dplyr::filter(country != "PHL") %>% 
  rbind(., df)

#### changes iso3 codes to country names
fish.surveys <- fish.surveys %>% 
  dplyr::mutate(
    country = dplyr::recode(country,
      "HND" = "Honduras",
      "IDN" = "Indonesia",
      "MOZ" = "Mozambique",
      )
  )


##### Jan 3, 2022 or so
### Fix location_status for some rows
fish.surveys$location_status[fish.surveys$location_status == "ma"] <- "Managed Access"
fish.surveys$location_status[fish.surveys$location_status == "MA"] <- "Managed Access"
fish.surveys$location_status[fish.surveys$location_status == "reserve"] <- "Reserve"
fish.surveys$location_status[fish.surveys$location_status == "outside"] <- "Managed Access"
fish.surveys$location_status[fish.surveys$location_status == "Outside"] <- "Managed Access"
fish.surveys$location_status[fish.surveys$location_status == "Inside"] <- "Reserve"
fish.surveys$location_status[fish.surveys$location_status == "TURF"] <- "Managed Access"

fish.surveys$year <- as.integer(fish.surveys$year)

##### Mar 2, 2022
### Fix level1_name in HND records so they match
fish.surveys$level1_name[fish.surveys$level1_name == "Islas de Bahía"] <- "Islas de la Bahía"

usethis::use_data(fish.surveys, overwrite = TRUE)

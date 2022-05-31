## code to prepare `benthic` dataset goes here

### add 2021 PHL data
library(dplyr)
library(readxl)

df_phl <- read_excel("../data/Benthic_survey_RARE_AllSites_2020-21_PHv2.xlsx")
detach(package:readxl)

# first, let's add the missing geo columns.
fish_coords <- fish.surveys %>% 
  select(ma_name, lat, lon) %>% 
  unique()

get_geo_names <- function(df, site, fish_coords) {
  site_coords <- df %>% 
    filter(sitename == site) %>% 
    select(lat, lon) %>% 
    unique()
  
  distances <- data.frame(ma_name = fish_coords$ma_name,
                          D = double(nrow(fish_coords))
  )
  distances$D <- (fish_coords$lat - site_coords$lat)**2 + (fish_coords$lon - site_coords$lon)**2
  
  closest_maa <- distances %>% 
    filter(D == min(D, na.rm = TRUE)) %>%  # the min distances are always 0
    pull(ma_name) %>% 
    unique()

  
  geo_names <- fish.surveys %>% 
    filter(ma_name == closest_maa) %>% 
    select(level1_name, level2_name, ma_name) %>% 
    unique()
  
  geo_names
}

sitenames <- unique(df_phl$sitename)

for (site in sitenames) {
  geo_names <- get_geo_names(df_phl, site, fish_coords)
  df_phl$level1_name[df_phl$sitename == site] <- geo_names$level1_name
  df_phl$level2_name[df_phl$sitename == site] <- geo_names$level2_name
  df_phl$ma_name[df_phl$sitename == site] <- geo_names$ma_name
}

df_phl$year <- 2021

## now pre-process IDN+HND data
## from BenthicMaster.csv in Benthic Surveys dataset
## https://data.world/rare/benthic-surveys/workspace/file?filename=BenthicMaster.csv
df <- read.csv("https://query.data.world/s/3bfxh2fxbexydjawr4q756hkt6av4v", header=TRUE, stringsAsFactors=FALSE);

df_sites <- df$sitename %>% unique()
df_sites[df_sites %in% fish.surveys$ma_name]

get_geo_names_by_string <- function(df, site) {
  # instead of using disance, match the sitename with ma_name, since there are matches
  # for almost all sitenames aside from HND's "Mangrove Bight"
  geo_names <- fish.surveys %>% 
    filter(ma_name == site) %>% 
    select(level1_name, level2_name, ma_name) %>% 
    unique()
  
  if (nrow(geo_names) > 1) {
    cat(paste(site, "has more than one matching set of geo names:"))
    cat(geo_names)
    return(NULL)
  } else {
    return(geo_names)
  }
}

for (site in df_sites) {
  geo_names <- get_geo_names_by_string(df, site)
  if (!is.null(geo_names)) {
    df$level1_name[df$sitename == site] <- geo_names$level1_name
    df$level2_name[df$sitename == site] <- geo_names$level2_name
    df$ma_name[df$sitename == site] <- geo_names$ma_name
  }
}

df <- df %>% 
  filter(country != "HND") # only 5 HND records and the sitename doesn't even match an maa

df$year[df$country == "MOZ"] <- 2020
df$year[df$country == "IDN"] <- 2019

cols <- intersect(names(df), names(df_phl))
# [1] "country"         "sitename"        "controlsite"     "controlsitename" "lat"            
# [6] "lon"             "surveydate"      "surveytime"      "locationname"    "locationstatus" 
# [11] "divername"       "temp_c"          "depth_m"         "reefslope"       "transectno"     
# [16] "avg_complexity"  "methodology"     "category"        "attribute"       "percentage"     
# [21] "level1_name"     "level2_name"     "ma_name"         "year"
# the only difference is df_phl has a `reef zone` column, which only has one value: 'crest'

df <- df %>% 
  select(cols)

df_phl <- df_phl %>% 
  select(cols)

benthic.surveys <- rbind(df, df_phl)

benthic.surveys <- benthic.surveys %>%
  mutate(country = case_when(
    country == "IDN" ~ "Indonesia",
    country == "MOZ" ~ "Mozambique",
    country == "PHL" ~ "Philippines"
    )) %>% 
  rename(location_status = locationstatus,
         location_name = locationname,
         transect_no = transectno)

detach(package:dplyr)

## Jan 12 2022
## 2021 Indonesia data
## From https://data.world/rare/benthic-surveys/
## se_sulawesi_monitoring_biofisik_2021-benthicpit-obs-20210818.csv
df <- read.csv2("../data/Benthic_IDN_SE-Sulawesi_2021.csv", header=TRUE, stringsAsFactors=FALSE);

df$level1_name <- "Southeast Sulawesi"

df <- df %>% 
  # there's 2019 data here but after grouping to get percentages (the next pipeline),
  # the 2019 data from BenthicMaster provides more samples for the same areas.
  # So, we ignore the 2019 data here
  # Also, the records with blank `Country` have missing benthic info as well, so
  # they are useless
  dplyr::filter(Country != "", Year != 2019) %>%
  dplyr::rename(
    country = Country,
    level2_name = District,
    ma_name = MAR.Name,
    location_name = Site,
    location_status = Management.name,
    transect_no = Transect.number,
    PIT_interval = PIT.interval..m.,
    lat = Latitude,
    lon = Longitude,
    year = Year,
    month = Month,
    day = Day,
    depth_m = Depth,
    reefslope = Reef.slope,
    category = Benthic.category,
    attribute = Benthic.attribute
  ) %>% 
  dplyr::mutate(
    year = as.character(year),
    month = as.character(month),
    day = as.character(day)
  ) %>% 
  tidyr::unite("surveydate", c(year, month, day), sep = "-", remove = FALSE) %>% 
  dplyr::select(
    country,
    level1_name,
    level2_name,
    ma_name,
    location_name,
    location_status,
    transect_no,
    PIT_interval,
    lat,
    lon,
    year,
    surveydate,
    depth_m,
    reefslope,
    category,
    attribute
  )

df <- df %>% 
  dplyr::group_by(year, location_name, transect_no, category, attribute) %>% 
  dplyr::mutate(percentage = dplyr::n()) %>% 
  dplyr::select(-PIT_interval) %>% 
  dplyr::distinct() %>% 
  dplyr::ungroup()

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

benthic.surveys <- rbind(benthic.surveys, df)
benthic.surveys$year <- as.integer(benthic.surveys$year)
benthic.surveys <- benthic.surveys %>% 
  dplyr::mutate(location_status = dplyr::case_when(
    location_status == "ma" ~ "Managed Access",
    location_status == "outside" ~ "Managed Access",
    location_status == "reserve" ~ "Reserve",
    location_status == "Managed Access" ~ "Managed Access",
    location_status == "Reserve" ~ "Reserve"
  ))


usethis::use_data(benthic.surveys, overwrite = TRUE)

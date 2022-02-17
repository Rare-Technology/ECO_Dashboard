## code to prepare `fish.surveys` dataset goes here
##
## fish_surveys_all from Dive Surveys project https://data.world/rare/dive-surveys/
fish.surveys <- readr::read_csv("https://query.data.world/s/khyask2eviivyjp4mbhyaufodpjmrl");

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
fish.surveys$ma_name[fish.surveys$ma_name == "Pilar"] <- "Pilar, Cebu" # based on matching lat/lon with PHL data below

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
# The 2017 data is made up entirely of Teluk Kolono records.
# The 2019 records differ from the existing ones by one row; we won't worry about them
# The 2021 records are almost the same but the count density is more accurate than the ones in fish.surveys
# since the weight-length formula had to be used in fish.surveys
# For this stage of data cleaning, we will:
# 1. Filter the 2021/2017 records from the updated dataset
# 2. Select appropriate columns from the updated dataset
# 3. Add level1_name/level2_name columns
# 4. Drop the 2021 IDN records in fish.surveys
# 5. Merge the new 2021/2017 records with fish.surveys
# 6. Update the .rda and document

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
  dplyr::filter(Year %in% c(2017, 2021)) %>% 
  # 2
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
  # 3
  dplyr::mutate(
    country = "IDN",
    survey_date = lubridate::ymd(paste(year, month, day, sep = "-")),
    size_class = as.character(get_size_class(length)),
    level1_name = get_level_name(ma_name, "level1_name"),
    level2_name = get_level_name(ma_name, "level2_name")
  )

fish.surveys <- fish.surveys %>% 
  # 4 drop lmax from fish.surveys
  dplyr::select(intersect(names(df), names(fish.surveys))) 

# drop month, day from df
df <- df %>% dplyr::select(names(fish.surveys))

# 5
fish.surveys <- rbind(fish.surveys, df)


#### 2021 PHL data
library(readxl)
library(rfishbase)

data_path <- "../data/PHL_fish_data_2021.xlsx"
df1 <- read_xlsx(data_path, sheet = 1)
df2 <- read_xlsx(data_path, sheet = 2)
df3 <- read_xlsx(data_path, sheet = 3)

# survey_date for df3 is not parsed correctly; almost all of the dates appear as
# character of an integer representing the number of days since a certain date in 1900
# eg "44332", "44317"

df2 <- df2 %>% 
  dplyr::rename(transect_no = "Transect number",
                transect_area = "Transect area")

df3 <- df3 %>% 
  dplyr::rename(transect_no = Transect,
                length = "length (cm)",
                transect_area = "Transect area (m2)")

clean_df <- function(df) {
  df %>% 
    dplyr::select(
      country,
      level1_name,
      level2_name,
      ma_name,
      location_name,
      location_status,
      lat = Latitude,
      lon = Longitude,
      transect_no,
      transect_area, # need to compute values of density_ind_ha
      count = Count,
      family,
      species = Species,
      length,
      size_class,
      reef_slope,
      reef_zone,
      survey_date
    ) %>% 
    dplyr::mutate(
      survey_date = as.character(survey_date), # save effort on fixing df3 problem
      density_ind_ha = count / transect_area * 10000 # m2 to ha
    )
}

df1 <- clean_df(df1)
df2 <- clean_df(df2)
df3 <- clean_df(df3)

df <- rbind(df1, rbind(df2, df3))


# Now that we have the three sheets combined into one data frame, we need to calculate the biomass
# To do this, we need the weight for each fish (weight_kg)
# To get the weight, we need to use the length-weight formula and thus need the a and b parameters
# The spreadsheet that df1/2/3 came from do have an a and b column, but they are entirely blank.
# To get those parameters, we pull from some tables we have.
# Not every species in df has an a or b value in the tables, so we impute missing values using
# the genus means of a/b.

# first, create a column for the genus. while we're at it, set the survey years
genus_regex <-  "^[A-Za-z]*"
df <- df %>% 
  dplyr::mutate(
    year = 2021,
    genus = stringr::str_extract(species, pattern = genus_regex)
  )

# a few records have genus Archamia but they were reclassified as Taeniamia in 2013
df[df$genus == "Archamia",]$genus <- "Taeniamia"
# fix some typos
df[df$species == "Stethjulis trilineata",]$genus <- "Stethojulis"
df[df$species == "chaetodon octofasciatus",]$genus <- "Chaetodon"


# read in the tables for a/b parameters
ab_old <- read.csv("https://query.data.world/s/adbk2l66cdg7vhd6h4ejq6n32ekq7a", header=TRUE, stringsAsFactors=FALSE);
ab <- read.csv("../data/fish_ab.csv")
ab <- ab %>% 
  dplyr::mutate(species = paste(Genus, Species)) %>% 
  dplyr::select(
    species,
    a = Biomass.Constant.A,
    b = Biomass.Constant.B
)

ab_old <- ab_old %>% 
  dplyr::select(
    species,
    a,
    b
  ) %>% 
  dplyr::filter(
    !(species %in% unique(ab$species))
)

ab <- rbind(ab, ab_old)

ab_species <- ab %>% 
  dplyr::filter(!is.na(a) & !is.na(b)) %>% 
  dplyr::group_by(species) %>%
  dplyr::mutate(a_species = mean(a), b_species = mean(b)) %>% # there is one species with two records, this line is for that
  dplyr::ungroup() %>%
  dplyr::select(species, a_species, b_species) %>% 
  dplyr::distinct()

ab_genus <- ab_species %>% 
  dplyr::mutate(genus = stringr::str_extract(species, pattern = genus_regex)) %>% 
  dplyr::group_by(genus) %>% 
  dplyr::mutate(a_genus = mean(a_species), b_genus = mean(b_species)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(genus, a_genus, b_genus) %>% 
  dplyr::distinct()

genfam <- fishbase %>%
  dplyr::select(genus = Genus, family = Family) %>% 
  dplyr::distinct()

df <- dplyr::left_join(df, ab_genus, by = "genus") %>% 
  dplyr::left_join(., ab_species, by = "species") %>% 
  dplyr::mutate(
    a = ifelse(is.na(a_species),
      a_genus,
      a_species),
    b = ifelse(is.na(b_species),
      b_genus,
      b_species),
    a_species = NULL,
    a_genus = NULL,
    b_species = NULL,
    b_genus = NULL
  ) %>% 
  dplyr::left_join(., genfam, by = "genus")

df <- df %>% 
  dplyr::mutate(
    weight_kg = a * length^b / 1000,
    biomass_kg_ha = density_ind_ha * weight_kg,
  ) %>% 
  dplyr::rename(
    family = family.y
  ) %>% 
  dplyr::select(
    -c(transect_area, genus, weight_kg, family.x)
  )

# Some inconvenience... the records for this PHL excel spreadsheet and from the original data.world
# file (the initial read in this file) have some overlap on dates. It's only four dates, but within
# each day, the way they overlap is different:

# 2020-11-08 is OK; nothing to change

# 2020-11-12: fish.surveys supersets df. drop the df data on this day
df <- df %>%
  dplyr::filter(survey_date != "2020-11-12")

# 2020-11-13: This one is complicated. Neither dataframe supersets the other. For now, as a simple
# but not clean solution, just drop the df records since fish.surveys has more
df <- df %>%
  dplyr::filter(survey_date != "2020-11-13")

# 2020-11-14: df supersets fish.surveys; drop fish.surveys records
fish.surveys <- fish.surveys %>% 
  dplyr::filter(!(country == "PHL" & survey_date == "2020-11-14"))

fish.surveys <- rbind(fish.surveys, df)

fish.surveys$year[fish.surveys$country == "PHL"] <- 2021
#### transform country names

iso3_to_full <- function(x) {
  switch(x,
         "HND" = "Honduras",
         "IDN" = "Indonesia",
         "MOZ" = "Mozambique",
         "PHL" = "Philippines")
}

fish.surveys$country <- as.character(sapply(fish.surveys$country, iso3_to_full))

#### 2021 PHL data compute size_class
# forgot to do this when first adding the data

get_size_class <- function (x) { # need to fix this for existing fish data, its a mess
  if (x <= 5) {"0-5"}
  else if (x > 5 & x <= 10) {"5-10"}
  else if (x > 10 & x <= 20) {"10-20"}
  else if (x > 20 & x <= 30) {"20-30"}
  else if (x > 30 & x <= 40) {"30-40"}
  else if (x > 40 & x <= 50) {"40-50"}
  else if (x > 50) {"50+"}
}
get_size_class <- Vectorize(get_size_class)

fish.surveys$size_class[fish.surveys$country == "Philippines"] <- 
  as.character(get_size_class(fish.surveys$length[fish.surveys$country == "Philippines"]))

### Jan 3, 2022 or so
#### Fix location_status for some rows
fish.surveys$location_status[fish.surveys$location_status == "MA"] <- "Managed Access"
fish.surveys$location_status[fish.surveys$location_status == "reserve"] <- "Reserve"
fish.surveys$location_status[fish.surveys$location_status == "outside"] <- "Managed Access"

fish.surveys$year <- as.integer(fish.surveys$year)

usethis::use_data(fish.surveys, overwrite = TRUE)

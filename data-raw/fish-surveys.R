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

# MOZ 2020 survey was at the end of the year and about 50 records (out of 504) have a survey date that
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

get_level_name <- function (x, level) {
  if (x %in% unique(fish.surveys$ma_name)) {
    fish.surveys %>% 
      dplyr::filter(ma_name == x) %>% 
      dplyr::pull(level) %>% 
      unique()
  } else if (x == "Teluk Kolono") {
    switch(level,
      "level1_name" = "Southeast Sulawesi",
      "level2_name" = "Konawe Selatan"
    )
  }
}
get_level_name <- Vectorize(get_level_name)

get_size_class <- function (x) { # need to fix this for existing fish data, its a mess
  if (x <= 5) {"0-5"}
  else if (x > 5 & x <= 10) {"5-10"}
  else if (x > 10 & x <= 20) {"10-20"}
  else if (x > 20 & x <= 30) {"20-30"}
  else if (x > 30 & x <= 40) {"30-40"}
  else if (x > 40 & x <= 50) {"40-50"}
}
get_size_class <- Vectorize(get_size_class)

df <- read.csv("../data/IDN_fish_data.csv", header = TRUE, stringsAsFactors = FALSE)
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
    ma_name = MAR.Name,
    location_name = Site,
    location_status = Management.name,
    transect_no = Transect.number,
    count = Count,
    family = Fish.family,
    species = Fish.taxon,
    density_ind_ha = density_countha,
    biomass_kg_ha = Biomass_kgha,
    length = Size
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
  # 4
  dplyr::filter(country != "IDN" | year != 2021) %>% 
  dplyr::select(intersect(names(df), names(fish.surveys)))

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
      species = Species,
      length,
      survey_date,
      size_class
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


# now that we have the three sheets combined into one data frame, we need to calculate the biomass
# to do this, we need the weight for each fish (weight_kg)
# to get the weight, we need to use the length-weight formula and thus need the a and b parameters
# to get those parameters, we pull from some tables we have.
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
    transect_area = NULL,
    a = NULL,
    b = NULL,
    genus = NULL,
  ) %>% 
  dplyr::mutate(
    weight_kg = NULL
  )

fish.surveys <- rbind(fish.surveys, df)

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

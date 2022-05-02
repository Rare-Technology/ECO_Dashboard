library(httr)
library(readxl)
library(dplyr)

# datasets from mangrove surveys project https://data.world/raremozambique/mangrove-surveys

# sapling data https://data.world/raremozambique/mangrove-surveys/workspace/file?filename=Mangrove_quadrat_Memba_Ihla_de_Moz_Dec2020_clean.csv
mangroves_sapling <- read.csv("https://query.data.world/s/xgeymqflnvsms2k27ysssgbo2pyw3a", header=TRUE, stringsAsFactors=FALSE);

# adult tree data https://data.world/raremozambique/mangrove-surveys/workspace/file?filename=Mangrove_plot_Memba_Ihla_de_Moz_Dec2020_clean.csv
mangroves_adult <- read.csv("https://query.data.world/s/b4ebnmh2n4fb4sy64v7uublj3ra5up", header=TRUE, stringsAsFactors=FALSE);

# geographic info https://data.world/raremozambique/mangrove-surveys/workspace/file?filename=Coordenate+dataBase_18.02.2021.xlsx
# GET("https://query.data.world/s/do73rt2rha43vbagracxgy6xovbq6h", write_disk(tf <- tempfile(fileext = ".xlsx")))
# geo <- read_excel(tf)

mangroves_adult <- mangroves_adult %>%
  mutate(age = "adult", quadrat_no = NA)
mangroves_adult <- mangroves_adult %>%
  dplyr::select(country,
    level1_name,
    level2_name,
    ma_name = level4_name,
    location_name = survey_location,
    location_status,
    transect_no,
    plot_no,
    quadrat_no,
    count = tree_no, # to get density
    tree_species, # to get diversity
    dbh_cm, # to get diameter
    age) %>% 
  dplyr::mutate(
    dbh_cm = as.numeric(dbh_cm)
  )

mangroves_adult[mangroves_adult==""] <- NA
mangroves_adult <- mangroves_adult[!apply(mangroves_adult, 1, function (x) all(is.na(x))),]
mangroves_adult$location_name[mangroves_adult$location_name == "Isla2 " &
                                !is.na(mangroves_adult$location_name)] <- "Isla2"
mangroves_adult <- mangroves_adult %>% filter(dbh_cm >= 4)

mangroves_sapling <- mangroves_sapling %>%
  mutate(age = "sapling", dbh_cm = NA)
mangroves_sapling[mangroves_sapling == ""] <- NA
mangroves_sapling[mangroves_sapling$plot_no == "`1" & !is.na(mangroves_sapling$plot_no), "plot_no"] <- 1
mangroves_sapling$plot_no <- as.integer(mangroves_sapling$plot_no)
mangroves_sapling <- mangroves_sapling %>% 
  dplyr::select(country,
    level1_name,
    level2_name,
    ma_name = level4_name,
    location_name = survey_location,
    location_status,
    transect_no,
    plot_no,
    quadrat_no,
    count,
    tree_species,
    dbh_cm,
    age
  ) %>% 
  dplyr::mutate(
    dbh_cm = as.numeric(dbh_cm)
  )
mangroves <- rbind(mangroves_adult, mangroves_sapling)


## Nov 12 2021
## For future proofing, will hard code the year of the above survey data (2020)
mangroves$year <- 2020

## Jan 2 2022
## Renaming for consistency with other datasets
mangrove.surveys <- mangroves

## May 1 2022
## 2020 Brazil Mangrove survey data %>% 
df_adult <- readxl::read_excel('../data/Mangrove-Surveys-Brazil.xlsx', sheet = 'master_plot')
df_adult <- df_adult[!apply(df_adult, 1, function (x) all(is.na(x))),]
df_adult <- df_adult %>% 
  dplyr::mutate(age = 'adult', quadrat_no = NA, year = 2020) %>% 
  dplyr::select(country,
    level1_name = `level1_name (state or region)`,
    level2_name = `level2_name (district)`,
    ma_name = `level4_name (village or ward)`,
    location_name = survey_location,
    location_status,
    transect_no,
    plot_no,
    quadrat_no,
    count = tree_no,
    tree_species,
    dbh_cm,
    age,
    year
  ) %>% 
  dplyr::filter(
    dbh_cm >= 4
  )

df_sapling <- readxl::read_excel('../data/Mangrove-Surveys-Brazil.xlsx', sheet = 'master_quadrat')
df_sapling <- df_sapling %>% 
  dplyr::mutate(age = 'sapling', dbh_cm = NA, year = 2020) %>% 
  dplyr::select(country,
    level1_name = `level1_name (state or region)`,
    level2_name = `level2_name (district)`,
    ma_name = `level4_name (village or ward)`,
    location_name = survey_location,
    location_status,
    transect_no,
    plot_no,
    quadrat_no,
    count, # all NA! density plot does not generate
    tree_species,
    dbh_cm,
    age,
    year
  )

mangrove.surveys <- rbind(mangrove.surveys, df_adult) %>% 
  rbind(., df_sapling)

mangrove.surveys$dbh_cm <- as.numeric(mangrove.surveys$dbh_cm)

usethis::use_data(mangrove.surveys, overwrite = TRUE)
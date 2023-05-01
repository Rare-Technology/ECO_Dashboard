library(httr)
library(readxl)
library(dplyr)
library(data.world)

mangroves_sapling <- data.world::query(
  data.world::qry_sql("SELECT * FROM mangrove_quadrat_memba_ihla_de_moz_dec2020_clean"),
  "https://data.world/raremozambique/mangrove-surveys/"
)


mangroves_adult <- data.world::query(
  data.world::qry_sql("SELECT * FROM mangrove_plot_memba_ihla_de_moz_dec2020_clean"),
  "https://data.world/raremozambique/mangrove-surveys/"
)

mangroves_adult <- mangroves_adult %>%
  mutate(age = "adult")
mangroves_adult <- mangroves_adult %>%
  dplyr::select(
    country,
    level1_name,
    level2_name,
    ma_name = level4_name,
    location_name = survey_location,
    location_status,
    transect_no,
    plot_no,
    count = tree_no, # to get density
    tree_species, # to get diversity
    dbh_cm, # to get diameter
    age
  ) %>% 
  dplyr::mutate(
    dbh_cm = as.double(dbh_cm)
  )

mangroves_adult[mangroves_adult==""] <- NA
mangroves_adult <- mangroves_adult[!apply(mangroves_adult, 1, function (x) all(is.na(x))),]
mangroves_adult$location_name[mangroves_adult$location_name == "Isla2 " &
                                !is.na(mangroves_adult$location_name)] <- "Isla2"
mangroves_adult <- mangroves_adult %>% filter(dbh_cm >= 4)

mangroves_sapling <- mangroves_sapling %>%
  mutate(age = "sapling")
mangroves_sapling[mangroves_sapling == ""] <- NA
mangroves_sapling <- mangroves_sapling %>% 
  dplyr::select(country,
    level1_name,
    level2_name,
    ma_name = level4_name,
    location_name = survey_location,
    location_status,
    transect_no,
    plot_no,
    count,
    tree_species,
    age
  ) %>% 
  dplyr::mutate(
    plot_no = stringr::str_extract(plot_no, "\\d") %>% as.integer()
  )

mangroves <- dplyr::bind_rows(mangroves_adult, mangroves_sapling)

mangroves <- mangroves %>%
  dplyr::mutate(
    year = 2020,
    ma_name = dplyr::recode(
      ma_name,
      "Memba" = "Memba-sede"
    )
  )

## Jan 2 2022
## Renaming for consistency with other datasets
mangrove.surveys <- mangroves

## May 1 2022
## 2020 Brazil Mangrove survey data %>% 
df_adult <- readxl::read_excel('../data/Mangrove-Surveys-Brazil.xlsx', sheet = 'master_plot')
df_adult <- df_adult[!apply(df_adult, 1, function (x) all(is.na(x))),]
df_adult <- df_adult %>% 
  dplyr::mutate(
    age = 'adult',
    year = 2020,
    ma_name = "RESEX Caeté-Taperaçu"
  ) %>% 
  dplyr::select(
    country,
    level1_name = `level1_name (state or region)`,
    level2_name = `level2_name (district)`,
    ma_name,
    location_name = survey_location,
    location_status,
    transect_no,
    plot_no,
    count = tree_no,
    tree_species,
    dbh_cm,
    age,
    year
  ) %>% 
  dplyr::mutate(
    # Ignore the "NAs introduced by coercion message", R is tripping
    dbh_cm = dplyr::case_when(
      dbh_cm == "-" | is.na(dbh_cm) ~ as.double(NA),
      TRUE ~ round(as.double(dbh_cm), 1)
    )
  ) %>%
  dplyr::filter(
    dbh_cm >= 4
  )

# In the mutate below, we set count = 1. This is because there is no count data recorded, but
# after asking Mayra about it, she said each row represents a count of 1.
df_sapling <- readxl::read_excel('../data/Mangrove-Surveys-Brazil.xlsx', sheet = 'master_quadrat')
df_sapling <- df_sapling %>% 
  dplyr::mutate(age = 'sapling', year = 2020, count = 1) %>%
  dplyr::select(
    country,
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
    age,
    year
  ) %>% 
  dplyr::mutate(
    ma_name = "RESEX Caeté-Taperaçu"
  )

mangrove.surveys <- mangrove.surveys %>% 
  dplyr::mutate(
    # Make transect_no types compatible; the values from BRA survey are characters
    transect_no = as.character(transect_no)
  ) %>% 
  dplyr::bind_rows(
    df_adult,
    df_sapling
  ) %>% 
  dplyr::distinct()

usethis::use_data(mangrove.surveys, overwrite = TRUE)
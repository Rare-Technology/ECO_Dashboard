library("httr")
library("readxl")
library(dplyr)

# datasets from mangrove surveys project https://data.world/raremozambique/mangrove-surveys

# sapling data https://data.world/raremozambique/mangrove-surveys/workspace/file?filename=Mangrove_quadrat_Memba_Ihla_de_Moz_Dec2020_clean.csv
mangroves_sapling <- read.csv("https://query.data.world/s/gn6c3qvsjqyz4moo6edltkbsqbii45", header=TRUE, stringsAsFactors=FALSE);

# adult tree data https://data.world/raremozambique/mangrove-surveys/workspace/file?filename=Mangrove_plot_Memba_Ihla_de_Moz_Dec2020_clean.csv
mangroves_adult <- read.csv("https://query.data.world/s/3w4azkfn7dwmfzdtfvn7iu3hu36k4r", header=TRUE, stringsAsFactors=FALSE);

# geographic info https://data.world/raremozambique/mangrove-surveys/workspace/file?filename=Coordenate+dataBase_18.02.2021.xlsx
# GET("https://query.data.world/s/do73rt2rha43vbagracxgy6xovbq6h", write_disk(tf <- tempfile(fileext = ".xlsx")))
# geo <- read_excel(tf)

mangroves_adult <- mangroves_adult %>%
  mutate(age = "adult", quadrat_no = NA)
mangroves_adult <- mangroves_adult %>% select(country,
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
                                              dbh_cm,
                                              age) # to get diameter
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
mangroves_sapling <- mangroves_sapling %>% select(country,
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
                              age)
mangroves <- rbind(mangroves_adult, mangroves_sapling)

usethis::use_data(mangroves, overwrite = TRUE)
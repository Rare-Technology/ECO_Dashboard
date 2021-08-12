#' init 
#'
#' @description Code used to generate initial values. For speed, keep commented.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
# INIT <-  list()
# INIT$CURRENT_TAB <- 'Coral Reefs'
# INIT$SEL_METRIC <- 'Fish Biomass'
# INIT$COUNTRY_CHOICES <-  c("Honduras" = "HND",
#                          "Indonesia" = "IDN",
#                          "Mozambique" = "MOZ",
#                          "Philippines" = "PHL")
# INIT$SEL_COUNTRY <-  'HND'
# INIT$DATA_FULL = fish.surveys %>% dplyr::filter(family != "")
# INIT$DATA_FILTERED = fish.surveys %>% dplyr::filter(country == INIT$SEL_COUNTRY)
# INIT$DATA_FILTERED <-  fish.surveys %>% dplyr::filter(country == INIT$SEL_COUNTRY)
# INIT$SEL_SUBNATIONAL <-  get_geo_choices(INIT$DATA_FILTERED, 'level1_name')
# INIT$SUBNATIONAL_CHOICES <-  get_geo_choices(INIT$DATA_FILTERED, 'level1_name')
# INIT$SEL_LOCAL <-  get_geo_choices(INIT$DATA_FILTERED, 'level2_name')
# INIT$LOCAL_CHOICES <-  get_geo_choices(INIT$DATA_FILTERED, 'level2_name')
# INIT$SEL_MAA <- get_geo_choices(INIT$DATA_FILTERED, 'ma_name')
# INIT$MAA_CHOICES <-  get_geo_choices(INIT$DATA_FILTERED, 'ma_name')
# INIT$SEL_FAMILY <- get_geo_choices(INIT$DATA_FILTERED, 'family')
# INIT$BASEMAP <- providers$Esri.OceanBasemap
# INIT$COORDS <- aggregate(cbind(lon, lat) ~ location_name,
#                           data=INIT$DATA_FULL, FUN=mean)
# usethis::use_data(INIT, overwrite=TRUE)
# devtools::document()


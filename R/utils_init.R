#' init 
#'
#' @description Code used to generate initial values. For speed, keep commented.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
# INIT <-  list()
# INIT$CURRENT_TAB <- 'Start'
# INIT$SEL_METRIC <- 'Fish Biomass'
# INIT$SEL_GEOM <- "Bar plots"
# INIT$SEL_YSCALE <- TRUE
# INIT$DATA_FULL <- list(
#   FISH = fish.surveys %>% dplyr::filter(family != ""), # this can be fixed easily...
#   MANGROVES = mangroves
# )
# INIT$YEAR_CHOICES <- list(
#   FISH = fish.surveys %>% dplyr::pull(year) %>% unique() %>% sort(),
#   MANGROVES = mangroves %>% dplyr::pull(year) %>% unique() %>% sort()
# )
# INIT$SEL_YEAR <- list(
#   FISH = max(INIT$YEAR_CHOICES$FISH),
#   MANGROVES = max(INIT$YEAR_CHOICES$MANGROVES)
# )
# INIT$DATA_FILTERED <- list(
#   FISH = INIT$DATA_FULL$FISH %>% dplyr::filter(year == INIT$SEL_YEAR$FISH),
#   MANGROVES = INIT$DATA_FULL$MANGROVES %>% dplyr::filter(year == INIT$SEL_YEAR$MANGROVES)
# )
# INIT$COUNTRY_CHOICES <- list(
#   FISH = get_geo_choices(INIT$DATA_FILTERED$FISH, "country"),
#   MANGROVES = get_geo_choices(INIT$DATA_FILTERED$MANGROVES, "country")
# )
# INIT$SEL_COUNTRY <- list(
#   FISH = INIT$COUNTRY_CHOICES$FISH[1],
#   MANGROVES = INIT$COUNTRY_CHOICES$MANGROVES[1]
# )
# INIT$DATA_FILTERED$FISH <- INIT$DATA_FILTERED$FISH %>%
#   dplyr::filter(country == INIT$SEL_COUNTRY$FISH)
# INIT$DATA_FILTERED$MANGROVES <- INIT$DATA_FILTERED$MANGROVES %>%
#   dplyr::filter(country == INIT$SEL_COUNTRY$MANGROVES)
# INIT$SEL_SUBNATIONAL <-  list(
#   FISH = get_geo_choices(INIT$DATA_FILTERED$FISH, 'level1_name'),
#   MANGROVES = get_geo_choices(INIT$DATA_FILTERED$MANGROVES, "level1_name")
# )
# INIT$SUBNATIONAL_CHOICES <- list(
#   FISH = get_geo_choices(INIT$DATA_FILTERED$FISH, 'level1_name'),
#   MANGROVES = get_geo_choices(INIT$DATA_FILTERED$MANGROVES, "level1_name")
# )
# INIT$SEL_LOCAL <- list(
#   FISH = get_geo_choices(INIT$DATA_FILTERED$FISH, 'level2_name'),
#   MANGROVES = get_geo_choices(INIT$DATA_FILTERED$MANGROVES, "level2_name")
# )
# INIT$LOCAL_CHOICES <- list(
#   FISH = get_geo_choices(INIT$DATA_FILTERED$FISH, 'level2_name'),
#   MANGROVES = get_geo_choices(INIT$DATA_FILTERED$MANGROVES, "level2_name")
# )
# INIT$SEL_MAA <- list(
#   FISH = get_geo_choices(INIT$DATA_FILTERED$FISH, 'ma_name'),
#   MANGROVES = get_geo_choices(INIT$DATA_FILTERED$MANGROVES, "ma_name")
# )
# INIT$MAA_CHOICES <- list(
#   FISH = get_geo_choices(INIT$DATA_FILTERED$FISH, 'ma_name'),
#   MANGROVES = get_geo_choices(INIT$DATA_FILTERED$MANGROVES, "ma_name")
# )
# INIT$SEL_FAMILY <- get_geo_choices(INIT$DATA_FILTERED$FISH, 'family')
# INIT$BASEMAP <- providers$Esri.OceanBasemap
# INIT$COORDS <- aggregate(cbind(lon, lat) ~ location_name,
#                           data=INIT$DATA_FULL$FISH, FUN=mean)
# usethis::use_data(INIT, overwrite=TRUE)

#
# note: do not put devtools::document here and think you can update init, usethis::use_data,
# and devtools::document all in one go.... because devtools::document is in this file, it will
# go in an infinite loop, continuously running devtools::document. Only use devtools::document
# in the console !

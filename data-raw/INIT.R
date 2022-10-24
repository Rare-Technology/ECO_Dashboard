## code to prepare `INIT` dataset goes here

#' init 
#'
#' @description Code used to generate initial values. For speed, keep commented.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
INIT <-  list()
INIT$DATASET <- list(
  CHOICES = sort(c("Fish", "Mangroves", "Benthic", "Seagrass", "Oysters")),#, "Crabs")),
  SELECTED = "Fish"
)
INIT$METRICS <- list(
  "Fish" = c('Fish biomass', 'Fish density', 'Fish diversity', 'Fish size'),
  "Mangroves" = c("Sapling density", "Tree diversity", "Tree size"),
  "Benthic" = c("Benthic diversity", "Benthic cover"),
  "Seagrass" = c("Seagrass cover", "Seagrass height"),
  "Oysters" = c("Oyster density", "Oyster size")
  #"Crabs" = c(Crab density", "Crab size")
)
INIT$CURRENT_TAB <- 'Start'
INIT$SEL_GEOM <- "Bar plots"
INIT$FACET_MAA <- TRUE
INIT$SEL_YSCALE <- TRUE
INIT$DATA_FULL <- list(
  "Fish" = fish.surveys %>% dplyr::filter(family != ""), # this can be fixed easily...
  "Mangroves" = mangrove.surveys,
  "Benthic" = benthic.surveys,
  "Seagrass" = seagrass.surveys,
  "Crabs" = crab.surveys,
  "Oysters" = oyster.surveys
)
INIT$COUNTRY$CHOICES <- get_geo_choices(INIT$DATA_FULL[["Fish"]], target = "country")
INIT$COUNTRY$SELECTED <- "Philippines"
INIT$SUBNATIONAL$CHOICES <- get_geo_choices(INIT$DATA_FULL[["Fish"]],
                              sel_country = INIT$COUNTRY$SELECTED,
                              target = "level1_name")
INIT$SUBNATIONAL$SELECTED <- INIT$SUBNATIONAL$CHOICES
INIT$LOCAL$CHOICES <- get_geo_choices(INIT$DATA_FULL[["Fish"]],
                                      sel_country = INIT$COUNTRY$SELECTED,
                                      sel_subnational = INIT$SUBNATIONAL$SELECTED,
                                      target = 'level2_name')
INIT$LOCAL$SELECTED <- INIT$LOCAL$CHOICES
INIT$MAA$CHOICES <- get_geo_choices(INIT$DATA_FULL[["Fish"]],
                                    sel_country = INIT$COUNTRY$SELECTED,
                                    sel_subnational = INIT$SUBNATIONAL$SELECTED,
                                    sel_local = INIT$LOCAL$SELECTED,
                                    target = "ma_name")
INIT$MAA$SELECTED <- INIT$MAA$CHOICES[1:2]
INIT$YEAR$CHOICES <- get_geo_choices(INIT$DATA_FULL[["Fish"]],
                                     sel_country = INIT$COUNTRY$SELECTED,
                                     sel_subnational = INIT$SUBNATIONAL$SELECTED,
                                     sel_local = INIT$LOCAL$SELECTED,
                                     sel_maa = INIT$MAA$SELECTED,
                                     target = "year")
INIT$YEAR$SELECTED <- max(INIT$YEAR$CHOICES)
INIT$FAMILY$CHOICES <- get_geo_choices(INIT$DATA_FULL[["Fish"]],
                                       sel_country = INIT$COUNTRY$SELECTED,
                                       sel_subnational = INIT$SUBNATIONAL$SELECTED,
                                       sel_local = INIT$LOCAL$SELECTED,
                                       sel_maa = INIT$MAA$SELECTED,
                                       sel_year = INIT$YEAR$SELECTED,
                                       target = 'family')
INIT$FAMILY$SELECTED <- INIT$FAMILY$CHOICES
INIT$MAA_YEAR <- INIT$DATA_FULL[["Fish"]] %>% 
  dplyr::select(ma_name, year) %>% 
  unique()
INIT$BASEMAP <- providers$Esri.OceanBasemap
INIT$COORDS <- aggregate(cbind(lon, lat) ~ location_name,
                         data=INIT$DATA_FULL[["Fish"]], FUN=mean)

#
# note: do not put devtools::document here and think you can update init, usethis::use_data,
# and devtools::document all in one go.... because devtools::document is in this file, it will
# go in an infinite loop, continuously running devtools::document. Only use devtools::document
# in the console !


usethis::use_data(INIT, overwrite = TRUE)

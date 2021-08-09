get_choices <- function(data_filtered, geo_level) {
  data_filtered %>% 
    dplyr::pull(geo_level) %>% 
    unique() %>% 
    as.vector() %>%
    sort()
}

initialize_geo <- function() {
  out = list()
  out$COUNTRY_CHOICES <-  c("Honduras" = "HND",
                           "Indonesia" = "IDN",
                           "Mozambique" = "MOZ",
                           "Philippines" = "PHL")
  out$SEL_COUNTRY <-  'HND'
  out$DATA_FILTERED <-  fish.surveys %>% dplyr::filter(country == out$SEL_COUNTRY)
  out$SEL_SUBNATIONAL <-  get_choices(out$DATA_FILTERED, 'level1_name')
  out$SUBNATIONAL_CHOICES <-  get_choices(out$DATA_FILTERED, 'level1_name')
  out$SEL_LOCAL <-  get_choices(out$DATA_FILTERED, 'level2_name')
  out$LOCAL_CHOICES <-  get_choices(out$DATA_FILTERED, 'level2_name')
  out$SEL_MAA <- get_choices(out$DATA_FILTERED, 'ma_name')
  out$MAA_CHOICES <-  get_choices(out$DATA_FILTERED, 'ma_name')
  
  out
}

get_biomass <- function(data_filtered) {
  aggregate(biomass_kg_ha ~ country + level1_name + level2_name + ma_name +
              location_name + location_status, data=data_filtered, FUN=sum)
}
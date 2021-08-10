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
  # add up the biomass of all fish at each transect, then take the means across
  # transects for each location, then take the means across locations for each
  # managed access area-location status combination
  
  aggregate(biomass_kg_ha ~ country + ma_name + location_status + location_name +
              transect_no, data=data_filtered, FUN=sum)
}

summarySE <- function(data_aggreg, metric) {
  
  data_loc_means <- aggregate(biomass_kg_ha ~ country + ma_name + location_status +
                             location_name, data=data_aggreg, FUN=mean)
  
  data_summary <- aggregate(biomass_kg_ha ~ country + ma_name + location_status,
                            data=data_loc_means, FUN=mean)
  data_summary$N <- aggregate(biomass_kg_ha ~ country + ma_name + location_status,
                               data=data_loc_means, FUN=length) %>% 
                      dplyr::pull(biomass_kg_ha)
  data_summary$SD <- aggregate(biomass_kg_ha ~ country + ma_name + location_status,
                               data=data_loc_means, FUN=sd) %>% 
                      dplyr::pull(biomass_kg_ha)
  data_summary$SE <- data_summary$SD / sqrt(data_summary$N)
  
  return(data_summary)
}
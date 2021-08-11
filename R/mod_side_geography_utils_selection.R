get_geo_choices <- function(data_filtered, geo_level) {
  data_filtered %>% 
    dplyr::pull(geo_level) %>% 
    unique() %>% 
    as.vector() %>%
    sort()
}

get_biomass <- function(data_filtered, metric) {
  # add up the biomass of all fish at each transect
  
  groupvars <- c('country', 'ma_name', 'location_status', 'lon', 'lat',
                 'location_name', 'transect_no')
  formula_str = paste(metric, paste(groupvars, collapse=" + "), sep=" ~ ")
  
  aggregate(formula=as.formula(formula_str), data=data_filtered, FUN=sum)
}
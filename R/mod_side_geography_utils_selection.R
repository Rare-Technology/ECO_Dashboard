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

get_biomass <- function(data_filtered, metric) {
  # add up the biomass of all fish at each transect
  
  groupvars <- c('country', 'ma_name', 'location_status', 'lon', 'lat',
                 'location_name', 'transect_no')
  formula_str = paste(metric, paste(groupvars, collapse=" + "), sep=" ~ ")
  
  aggregate(formula=as.formula(formula_str), data=data_filtered, FUN=sum)
}

summarySE <- function(data_aggreg, metric) {
  
  groupvars1 <- c('country', 'ma_name', 'location_status', 'location_name')
  groupvars2 <- groupvars1[-length(groupvars1)]
  
  # metric ~ country + ma_name + location_status + location_name
  formula1 <- paste(metric, paste(groupvars1, collapse=" + "), sep=" ~ ") %>% 
                as.formula()
  # metric ~ country + ma_name + location_status
  formula2 <- paste(metric, paste(groupvars2, collapse=" + "), sep=" ~ ") %>% 
                as.formula()
  
  data_loc_means <- aggregate(formula1, data=data_aggreg, FUN=mean)
  data_summary <- aggregate(formula2, data=data_loc_means, FUN=mean)
  data_summary$N <- aggregate(formula2, data=data_loc_means, FUN=length) %>% 
                      dplyr::pull(metric)
  data_summary$SD <- aggregate(formula2, data=data_loc_means, FUN=sd) %>% 
                      dplyr::pull(metric)
  data_summary$SE <- data_summary$SD / sqrt(data_summary$N)
  
  return(data_summary)
  # data_loc_means <- aggregate(metric ~ country + ma_name + location_status +
  #                            location_name, data=data_aggreg, FUN=mean)
  # 
  # data_summary <- aggregate(metric ~ country + ma_name + location_status,
  #                           data=data_loc_means, FUN=mean)
  # data_summary$N <- aggregate(metric ~ country + ma_name + location_status,
  #                              data=data_loc_means, FUN=length) %>% 
  #                     dplyr::pull(metric)
  # data_summary$SD <- aggregate(metric ~ country + ma_name + location_status,
  #                              data=data_loc_means, FUN=sd) %>% 
  #                     dplyr::pull(metric)
  # data_summary$SE <- data_summary$SD / sqrt(data_summary$N)
  
  return(data_summary)
}
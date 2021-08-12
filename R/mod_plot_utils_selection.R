aggregate_data <- function(data_filtered, metric) {
  # add up the biomass of all fish at each transect
  if (metric %in% c('biomass_kg_ha', 'density_ind_ha')) {
    groupvars <- c('country', 'ma_name', 'location_status', 'location_name', 'transect_no')
    formula_str <- paste(metric, paste(groupvars, collapse = ' + '), sep = ' ~ ')
    aggregate(as.formula(formula_str), data=data_filtered, FUN=sum)
    
  } else if (metric == 'species') {
    aggregate(species ~ country + ma_name + location_status +
                location_name,
                data=data_filtered, FUN=count_unique)
  }
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
  
  if (metric == 'species') {
    # in this case, data_aggreg is already aggregated by location,
    # by count_unique instead of mean
    data_loc <- data_aggreg
  } else {
    data_loc <- aggregate(formula1, data=data_aggreg, FUN=mean)
  }
  data_summary <- aggregate(formula2, data=data_loc, FUN=mean)
  
  data_summary$N <- aggregate(formula2, data=data_loc, FUN=length) %>% 
    dplyr::pull(metric)
  data_summary$SD <- aggregate(formula2, data=data_loc, FUN=sd) %>% 
    dplyr::pull(metric)
  data_summary$SE <- data_summary$SD / sqrt(data_summary$N)
  
  return(data_summary)
}

count_unique <- function(x) {
  length(unique(x))
}
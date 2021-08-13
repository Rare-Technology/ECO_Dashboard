aggregate_data <- function(data_filtered, metric) {
  # add up the biomass of all fish at each transect
  if (metric %in% c('biomass_kg_ha', 'density_ind_ha')) {
    groupvars <- c('country', 'ma_name', 'location_status', 'location_name',
                   'transect_no')
    formula_str <- paste(metric, paste(groupvars, collapse = ' + '), sep = ' ~ ')
    aggregate(as.formula(formula_str), data=data_filtered, FUN=sum)
    
  } else if (metric == 'species') {
    # note that unlike biomss and density, the data for species is aggregated
    # up to location_name. This is accounted for in later functions
    # such as get_map_data
    aggregate(species ~ country + ma_name + location_status +
                location_name,
                data=data_filtered, FUN=count_unique)
  } else if (metric == 'sizeclass') {
    # idea: for each size class, you can expect to see `density_ind_ha` many 
    # fish per hectare.
    aggregate(density_ind_ha ~ country + ma_name + location_status + location_name + 
                transect_no + sizeclass, data=data_filtered, FUN=sum)
  }
}

get_local_data <- function(data_aggreg, metric) {
  # this is only needed for biomass, density, and size, since in aggregate_data(),
  # aggregation by transects is already given for diversity
  
  if (metric %in% c('biomass_kg_ha', 'density_ind_ha')) {
    groupvars <- c('country', 'ma_name', 'location_status', 'location_name')
    formula <- paste(metric, paste(groupvars, collapse=" + "), sep=" ~ ") %>% 
      as.formula()
    aggregate(formula, data=data_aggreg, FUN=mean)
  } else { # sizeclass
    aggregate(density_ind_ha ~ country + ma_name + location_status +
                location_name + sizeclass, data=data_aggreg, FUN=mean)
  }
}
summarySE <- function(data_aggreg, metric, for.size=FALSE) {
  
  groupvars1 <- c('country', 'ma_name', 'location_status', 'location_name')
  groupvars2 <- groupvars1[-length(groupvars1)]
  if (for.size) {
    groupvars1 <- append(groupvars1, 'sizeclass')
    groupvars2 <- append(groupvars2, 'sizeclass')
    # changing metric to 'count' to use in the following aggregates w/o changing
    # the code much. maybe clunky to do so and it may be better to just set metric
    # to 'count' from the beginning, but from a readability/comprehension standpoint,
    # I think it makes sense to keep metric as 'sizeclass' in the beginning,
    # as it makes it very clear that the surrounding code is in regard to fish size
  }
  
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
    # take mean across transects
    data_loc <- aggregate(formula1, data=data_aggreg, FUN=mean)
  }
  # take mean across locations
  data_summary <- aggregate(formula2, data=data_loc, FUN=mean)
  
  if (!for.size) { # ie not sizeclass
    data_summary$N <- aggregate(formula2, data=data_loc, FUN=length) %>% 
      dplyr::pull(metric)
    data_summary$SD <- aggregate(formula2, data=data_loc, FUN=sd) %>% 
      dplyr::pull(metric)
    data_summary$SE <- data_summary$SD / sqrt(data_summary$N)
  }  
  
  return(data_summary)
}

count_unique <- function(x) {
  length(unique(x))
}
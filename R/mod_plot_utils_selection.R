aggregate_data <- function(data_filtered, metric) {
  # add up the biomass of all fish at each transect
  if (metric %in% c('biomass_kg_ha', 'density_ind_ha')) {
    groupvars <- c('country', 'ma_name', 'location_status', 'location_name',
                   'transect_no')
    formula_str <- paste(metric, paste(groupvars, collapse = ' + '), sep = ' ~ ')
    aggregate(as.formula(formula_str), data=data_filtered, FUN=sum)
    
  } else if (metric == 'species') {
    # note that unlike biomass and density, the data for species is aggregated
    # up to location_name. This is accounted for in later functions
    # such as get_map_data
    aggregate(species ~ country + ma_name + location_status + location_name,
                data=data_filtered, FUN=count_unique)
    
  } else if (metric == "tree_species") {
    # looks just like aggregating fish species but may have to do something later
    # regarding adult vs sapling. if that's not the case, consider changing
    # the column name tree_species to simply species
    aggregate(tree_species ~ country + ma_name + location_status + location_name,
              data = data_filtered, FUN = count_unique)
    
  } else if (metric == "dbh_cm") {
    data_filtered <- data_filtered %>% # these lines may change depending on how
      dplyr::filter(age == "adult") # age is implemented in other metrics
    aggregate(dbh_cm ~ country + ma_name + location_status + location_name +
                transect_no + plot_no,
              data = data_filtered,
              FUN = mean) %>% 
      aggregate(dbh_cm ~ country + ma_name + location_status + location_name +
                  transect_no,
                data = .,
                FUN = mean)
    
  } else if (metric == "sapling_tree_density_ind_m2") {
    # first sum up the trees in a quadrat, relabel this quantity to sapling_tree_density_ind_m2,
    # then take the mean across the quadrats, then the mean across the plots.
    data_filtered <- data_filtered %>%
      dplyr::filter(age == "sapling")
    aggregate(count ~ country + ma_name + location_status + location_name +
                transect_no + plot_no + quadrat_no,
              data = data_filtered,
              FUN = sum) %>% 
      dplyr::rename(sapling_tree_density_ind_m2 = count) %>%
      aggregate(sapling_tree_density_ind_m2 ~ country + ma_name + location_status + location_name +
                  transect_no + plot_no,
                data = .,
                FUN = mean) %>% 
      aggregate(sapling_tree_density_ind_m2 ~ country + ma_name + location_status + location_name +
                  transect_no,
                data = .,
                FUN = mean)
    
  } else if (metric == 'size_class') {
    # idea: for each size class, you can expect to see `density_ind_ha` many 
    # fish per hectare.
    aggregate(density_ind_ha ~ country + ma_name + location_status + location_name + 
                transect_no + size_class, data=data_filtered, FUN=sum)
  }
}

get_local_data <- function(data_aggreg, metric, for.size=FALSE) {
  # this is only needed for biomass, density, and size, since in aggregate_data(),
  # aggregation by transects is already given for diversity
  
  if (metric %in% c("biomass_kg_ha", "density_ind_ha", "dbh_cm", "sapling_tree_density_ind_m2")) {
    groupvars <- c('country', 'ma_name', 'location_status', 'location_name')
    if (for.size) {
      groupvars <- append(groupvars, 'size_class')
    }
    formula <- paste(metric, paste(groupvars, collapse=" + "), sep=" ~ ") %>% 
      as.formula()
    aggregate(formula, data=data_aggreg, FUN=mean)
  } else { # size_class
    aggregate(density_ind_ha ~ country + ma_name + location_status +
                location_name + size_class, data=data_aggreg, FUN=mean)
  }
}
summarySE <- function(data_aggreg, metric, for.size=FALSE, for.tree=FALSE) {
  
  groupvars1 <- c('country', 'ma_name', 'location_status', 'location_name')
  groupvars2 <- groupvars1[-length(groupvars1)]
  if (for.size) {
    groupvars1 <- append(groupvars1, 'size_class')
    groupvars2 <- append(groupvars2, 'size_class')
    # changing metric to 'count' to use in the following aggregates w/o changing
    # the code much. maybe clunky to do so and it may be better to just set metric
    # to 'count' from the beginning, but from a readability/comprehension standpoint,
    # I think it makes sense to keep metric as 'size_class' in the beginning,
    # as it makes it very clear that the surrounding code is in regard to fish size
  }
  
  # metric ~ country + ma_name + location_status + location_name
  formula1 <- paste(metric, paste(groupvars1, collapse=" + "), sep=" ~ ") %>% 
    as.formula()
  # metric ~ country + ma_name + location_status
  formula2 <- paste(metric, paste(groupvars2, collapse=" + "), sep=" ~ ") %>% 
    as.formula()
  
  if (metric %in%  c("species", "tree_species")) {
    # in this case, data_aggreg is already aggregated by location,
    # by count_unique instead of mean
    data_loc <- data_aggreg
  } else {
    # take mean across transects
    data_loc <- aggregate(formula1, data=data_aggreg, FUN=mean)
  }
  # take mean across locations
  data_summary <- aggregate(formula2, data=data_loc, FUN=mean)
  
  if (!for.size) { # ie not size_class
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
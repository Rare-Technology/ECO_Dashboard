#' Helper functions for data aggregation and summary statistics
#' 
#' @import dplyr
#' @import tidyr
aggregate_data <- function(data_filtered, metric) {
  # add up the biomass of all fish at each transect
  if (metric %in% c('biomass_kg_ha', 'density_ind_ha')) {
    groupvars <- c('country', 'ma_name', 'year',  'location_status', 'location_name',
                   'transect_no')
    formula_str <- paste(metric, paste(groupvars, collapse = ' + '), sep = ' ~ ')
    aggregate(as.formula(formula_str), data=data_filtered, FUN=sum)
    
  } else if (metric == 'species') {
    # note that unlike biomass and density, the data for species is aggregated
    # up to location_name. This is accounted for in later functions
    # such as get_map_data
    aggregate(species ~ year + country + ma_name + location_status + location_name,
                data=data_filtered, FUN=count_unique)
    
  } else if (metric == "tree_species") {
    # looks just like aggregating fish species but may have to do something later
    # regarding adult vs sapling. if that's not the case, consider changing
    # the column name tree_species to simply species
    aggregate(tree_species ~ year + country + ma_name + location_status + location_name,
              data = data_filtered, FUN = count_unique)
    
  } else if (metric == "attribute") {
    # many rows have attributes w/ percentage = 0. these should not be be part of the
    # diversity count
    aggregate(attribute ~ year + country + ma_name + location_status + location_name,
            data = data_filtered %>% dplyr::filter(percentage > 0), FUN = count_unique)
    
  } else if (metric == "cover") {
    data_filtered %>% 
      dplyr::group_by(year, country, ma_name, location_status, location_name,
               seagrass_species, transect_no) %>% 
      dplyr::summarize(cover = mean(cover, na.rm=TRUE)) %>% 
      dplyr::group_by(year, country, ma_name, location_status, location_name, seagrass_species) %>% 
      dplyr::summarize(cover = mean(cover)) %>% 
      dplyr::group_by(year, country, ma_name, location_status, seagrass_species) %>% 
      dplyr::summarize(cover = mean(cover))
      
  } else if (metric == "avg_height_cm") {
    data_filtered %>% 
      dplyr::filter(category == "Seagrass") %>% # maybe subject to change on future data
      dplyr::group_by(year, country, ma_name, location_status, location_name, transect_no) %>% 
      dplyr::summarize(avg_height_cm = mean(avg_height_cm, na.rm = TRUE))
    
  } else if (metric == "percentage") {
    data_filtered %>% 
      dplyr::group_by(year, country, ma_name, location_status, location_name,
                      transect_no, category) %>% 
      dplyr::summarize(percentage = sum(percentage)) %>% 
      dplyr::mutate(total_percentage = sum(percentage)) %>%
      dplyr::mutate(percentage = percentage / total_percentage * 100) %>% 
      dplyr::group_by(year, country, ma_name, location_status, location_name, category) %>%
      dplyr::summarize(percentage = mean(percentage)) %>%
      aggregate(percentage ~ country + ma_name + year + location_status + category,
                data = ., FUN = mean) %>% 
      dplyr::group_by(country, ma_name, year, location_status) %>% 
      dplyr::mutate(total_percentage = sum(percentage)) %>% 
      dplyr::mutate(percentage = percentage / total_percentage * 100)


    # aggregate(percentage ~ year + country + ma_name + location_status + location_name
    #           + category, data = data_filtered, FUN = mean)
    
  } else if (metric == "dbh_cm") {
    data_filtered <- data_filtered %>% # these lines may change depending on how
      dplyr::filter(age == "adult") # age is implemented in other metrics
    aggregate(dbh_cm ~ year + country + ma_name + location_status + location_name +
                transect_no + plot_no,
              data = data_filtered,
              FUN = mean) %>% 
      aggregate(dbh_cm ~ year + country + ma_name + location_status + location_name +
                  transect_no,
                data = .,
                FUN = mean)
    
  } else if (metric == "sapling_tree_density_ind_m2") {
    # first sum up the trees in a quadrat, relabel this quantity to sapling_tree_density_ind_m2,
    # then take the mean across the quadrats, then the mean across the plots.
    data_filtered <- data_filtered %>%
      dplyr::filter(age == "sapling")
    aggregate(count ~ year + country + ma_name + location_status + location_name +
                transect_no + plot_no + quadrat_no,
              data = data_filtered,
              FUN = sum) %>% 
      dplyr::rename(sapling_tree_density_ind_m2 = count) %>%
      aggregate(sapling_tree_density_ind_m2 ~ year + country + ma_name + location_status + location_name +
                  transect_no + plot_no,
                data = .,
                FUN = mean) %>% 
      aggregate(sapling_tree_density_ind_m2 ~ year +country + ma_name + location_status + location_name +
                  transect_no,
                data = .,
                FUN = mean)
    
  } else if (metric == 'length') {
    # data_filtered %>% 
    #   dplyr::filter(!is.na(length)) %>% 
    #   dplyr::group_by(year, country, ma_name, location_status, location_name, transect_no) %>% 
    #   dplyr::summarize(length = sum(count * length), count = sum(count))
    data_filtered %>% 
      dplyr::filter(!is.na(length)) %>% 
      dplyr::select(year, country, ma_name, location_status, location_name, transect_no, count, length) %>% 
      tidyr::uncount(weights = count) %>% 
      dplyr::group_by(year, country, ma_name, location_status, location_name, transect_no) %>% 
      dplyr::summarize(length = mean(length))
  }
}

get_local_data <- function(data_aggreg, metric, for.size=FALSE) {
  # this is only needed for biomass, density, and size, since in aggregate_data(),
  # aggregation by transects is already given for diversity
  
  if (metric %in% c("biomass_kg_ha", "density_ind_ha", "dbh_cm", "sapling_tree_density_ind_m2")) {
    groupvars <- c('country', 'year', 'ma_name', 'location_status', 'location_name')
    if (for.size) {
      groupvars <- append(groupvars, 'size_class')
    }
    formula <- paste(metric, paste(groupvars, collapse=" + "), sep=" ~ ") %>% 
      as.formula()
    aggregate(formula, data=data_aggreg, FUN=mean)
  } else { # size_class
    aggregate(density_ind_ha ~ country + year + ma_name + location_status +
                location_name + size_class, data=data_aggreg, FUN=mean)
  }
}
summarySE <- function(data_aggreg, metric, for.size=FALSE, for.tree=FALSE) {
  
  groupvars1 <- c('country', 'ma_name', 'year', 'location_status', 'location_name')
  groupvars2 <- groupvars1[-length(groupvars1)]
  if (for.size) {
    # data_local <- data_aggreg %>% 
    #   filter(!is.na(length)) %>% 
    #   dplyr::group_by(year, country, ma_name, location_status, location_name) %>% 
    #   dplyr::summarize(length = sum(count * length), count = sum(count))
    # data_summary <- data_local %>% 
    #   dplyr::group_by(year, country, ma_name, location_status) %>% 
    #   dplyr::summarize(length = sum(count * length), count = sum(count)) %>% 
    #   dplyr::rename(N = count)
    data_loc <- data_aggreg %>% 
      dplyr::filter(!is.na(length)) %>% 
      dplyr::group_by(year, country, ma_name, location_status, location_name) %>% 
      dplyr::summarize(length = mean(length))
    data_summary <- data_loc %>% 
      dplyr::group_by(year, country, ma_name, location_status) %>% 
      dplyr::summarize(length = mean(length))

    # return(data_summary)
  }
  
  if (metric == "percentage") {
    groupvars1 <- c(groupvars1, "category")
    groupvars2 <- c(groupvars2, "category")
  }  
  # metric ~ country + ma_name + year + location_status + location_name
  formula1 <- paste(metric, paste(groupvars1, collapse=" + "), sep=" ~ ") %>% 
    as.formula()
  # metric ~ country + ma_name + year + location_status
  formula2 <- paste(metric, paste(groupvars2, collapse=" + "), sep=" ~ ") %>% 
    as.formula()
  
  if (!for.size) {
    if (metric %in% c("species", "tree_species", "attribute", "percentage")) {
      # in this case, data_aggreg is already aggregated by location,
      # by count_unique instead of mean
      data_loc <- data_aggreg
    } else {
      # take mean across transects
      data_loc <- aggregate(formula1, data=data_aggreg, FUN=mean)
    }
    # take mean across locations
    data_summary <- aggregate(formula2, data=data_loc, FUN=mean)
  }
    data_summary$N <- aggregate(formula2, data=data_loc, FUN=length) %>% 
      dplyr::pull(metric)
    data_summary$SD <- aggregate(formula2, data=data_loc, FUN=sd) %>% 
      dplyr::pull(metric)
    data_summary$SE <- data_summary$SD / sqrt(data_summary$N)
    data_summary <- data_summary %>% 
      dplyr::mutate(ymin = !!sym(metric) - SE,
                    ymax = !!sym(metric) + SE)
  
  return(data_summary)
}

count_unique <- function(x) {
  length(unique(x))
}
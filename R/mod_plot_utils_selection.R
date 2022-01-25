#' Helper functions for data aggregation and summary statistics
#' 
#' @import dplyr
#' @import tidyr
aggregate_data <- function(data_filtered, metric) {
  # almost every metric will use at least these grouping variables
  groupvars <- c('ma_name', 'year', 'location_status', 'location_name', 'transect_no')
  
  if (metric %in% c('biomass_kg_ha', 'density_ind_ha')) {
    form <- str_to_formula(metric, groupvars)
    aggregate(form, data = data_filtered, FUN = sum)
    
  } else if (metric %in% c('species', 'tree_species', 'attribute')) {
    # for these metrics, we count unique values among all transects, so transect_no
    # is not a grouping variable here
    groupvars <- groupvars[-length(groupvars)] # drop transect_no
    form <- str_to_formula(metric, groupvars)
  
    # have to filter out some data for plotting attribute. other metrics OK
    if (metric == 'attribute') {
      data_filtered <- data_filtered %>% 
        dplyr::filter(percentage > 0)
    }
    
    aggregate(form, data = data_filtered, FUN = count_unique)
    
  } else if (metric == "cover") {
    # seagrass cover is among a few metrics that having been implemented later in dev,
    # I sought to `dplyr`-ize the code instead of using `aggregate`. in doing so though,
    # it is hard to follow the process of the past few metrics of creating a list
    # of grouping variables and passing into and pasting it into a formula.
    # At some point, figure out a way to write this better
    # Angel, Jan-24-22
    
    data_filtered %>% 
      dplyr::group_by(year, ma_name, location_status, location_name,
                      seagrass_species, transect_no) %>% 
      dplyr::summarize(cover = mean(cover, na.rm=TRUE)) %>% 
      dplyr::group_by(year, ma_name, location_status, location_name, seagrass_species) %>% 
      dplyr::summarize(cover = mean(cover)) %>% 
      dplyr::group_by(year, ma_name, location_status, seagrass_species) %>% 
      dplyr::summarize(cover = mean(cover))
      
  } else if (metric == "avg_height_cm") {
    data_filtered %>% 
      dplyr::filter(category == "Seagrass") %>% 
      dplyr::group_by(year, ma_name, location_status, location_name, transect_no) %>% 
      dplyr::summarize(avg_height_cm = mean(avg_height_cm, na.rm = TRUE))
    
  } else if (metric == "percentage") {
    data_filtered %>% 
      dplyr::group_by(year, ma_name, location_status, location_name,
                      transect_no, category) %>% 
      dplyr::summarize(percentage = sum(percentage)) %>% 
      dplyr::mutate(total_percentage = sum(percentage)) %>% # every row up to the same category will have the same sum
      dplyr::mutate(percentage = percentage / total_percentage * 100) %>% 
      dplyr::group_by(year, ma_name, location_status, location_name, category) %>%
      dplyr::summarize(percentage = mean(percentage)) %>%
      aggregate(percentage ~ ma_name + year + location_status + category,
                data = ., FUN = mean) %>% 
      dplyr::group_by(ma_name, year, location_status) %>% 
      dplyr::mutate(total_percentage = sum(percentage)) %>% 
      dplyr::mutate(percentage = percentage / total_percentage * 100)
    
  } else if (metric == "dbh_cm") {
    data_filtered <- data_filtered %>% # these lines may change depending on how
      dplyr::filter(age == "adult") # age is implemented in other metrics
    
    groupvars <- c(groupvars, 'transect_no', 'plot_no')
    groupvars2 <- groupvars[-length(groupvars)] # drop plot_no
    form <- str_to_formula(metric, groupvars)
    form2 <- str_to_formula(metric, groupvars2)
    
    aggregate(form, data = data_filtered, FUN = mean) %>% 
      aggregate(form2, data = ., FUN = mean)
    
  } else if (metric == "sapling_tree_density_ind_m2") {
    # first sum up the trees in a quadrat, relabel this quantity to sapling_tree_density_ind_m2,
    # then take the mean across the quadrats, then the mean across the plots.
    data_filtered <- data_filtered %>%
      dplyr::filter(age == "sapling")
    
    groupvars <- c(groupvars, 'transect_no', 'plot_no', 'quadrat_no')
    groupvars2 <- groupvars[-length(groupvars)] # drop quadrat_no
    groupvars3 <- groupvars2[-length(groupvars2)] # drop plot_no
    form <- str_to_formula('count', groupvars) # dependent var gets renamed for the 2nd/3rd aggregates
    form2 <- str_to_formula(metric, groupvars2)
    form3 <- str_to_formula(metric, groupvars3)
    
    aggregate(form, data = data_filtered, FUN = sum) %>% 
      dplyr::rename(sapling_tree_density_ind_m2 = count) %>%
      aggregate(form2, data = ., FUN = mean) %>% 
      aggregate(form3, data = ., FUN = mean)
    
  } else if (metric == 'length') {
    data_filtered %>% 
      dplyr::filter(!is.na(length)) %>% 
      dplyr::select(year, ma_name, location_status, location_name, transect_no, count, length) %>% 
      tidyr::uncount(weights = count) %>% 
      dplyr::group_by(year, ma_name, location_status, location_name, transect_no) %>% 
      dplyr::summarize(length = mean(length))
  }
}

get_local_data <- function(data_aggreg, metric, facet_maa) {
  # return one level above the final plotted data to use in distribution plot.
  #
  # in summarySE we will return a dataframe, data_summary, with this nested structure:
  # data_summary: year | ma_name | location_status | metric
  # get_local_data will return one level above that:
  # data_loc: year | ma_name | location_status | location_name | metric
  #
  # if facet_maa is false, both dataframes go one level lower:
  # data_summary: year | location_status | metric
  # data_loc: year | ma_name | location_status | metric (this is just data_summary when facet_maa is true)
  
  groupvars <- c('ma_name', 'year', 'location_status', 'location_name')
  form <- str_to_formula(metric, groupvars)
  data_loc <- aggregate(form, data = data_aggreg, FUN = mean)
  
  if (!facet_maa) {
    groupvars <- groupvars[-length(groupvars)]
    form <- str_to_formula(metric, groupvars)
    data_loc <- aggregate(form, data = data_loc, FUN = mean)
  }
  
  data_loc
}

summarySE <- function(data_aggreg, metric, facet_maa) {
  
  groupvars1 <- c('ma_name', 'year', 'location_status', 'location_name')
  groupvars2 <- groupvars1[-length(groupvars1)] # drop location_name
  
  if (metric == "percentage") {
    groupvars1 <- c(groupvars1, "category")
    groupvars2 <- c(groupvars2, "category")
  }
  
  # metric ~ ma_name + year + location_status + location_name (aggregate across transects)
  form1 <- str_to_formula(metric, groupvars1)
  # metric ~ ma_name + year + location_status (aggregate across survey sites)
  form2 <- str_to_formula(metric, groupvars2)

  if (metric %in% c("species", "tree_species", "attribute", "percentage")) {
    # data_loc is nested up to location_name. the metrics from this conditional
    # already have data_aggreg nested up to location_name, so let data_loc equal that
    data_loc <- data_aggreg
    
  } else {
    # all other metrics have data_aggreg nested up to transect_no, so aggregate
    # across transects and call it data_loc
    data_loc <- aggregate(form1, data = data_aggreg, FUN = mean)
    
  }
    
  # take mean across locations
  data_summary <- aggregate(form2, data = data_loc, FUN = mean)
  
  # recap:
  # data_loc looks like this: year | ma_name | location_status | location_name | metric
  # data_summary looks like this: year | ma_name | location_status | metric
  # if facet wrapping by MA is on, then the plots will display data_summary,
  # and if show distribution is on, then the data_loc points are added on top of that
  
  if (facet_maa) {
    data_summary$N <- aggregate(form2, data = data_loc, FUN = length) %>% 
      dplyr::pull(metric)
    data_summary$SD <- aggregate(form2, data = data_loc, FUN = sd) %>% 
      dplyr::pull(metric)
    data_summary$SE <- data_summary$SD / sqrt(data_summary$N)

  } else {
    # without facet wrapping by maa, we want to aggregate across ma_name. so we will
    # have data_loc and data_sumamry look like this:
    # data_loc: year | ma_name | location_status | metric
    # data_summary: year | location_status | metric
    
    groupvars3 <- groupvars2[-1]
    # metric ~ year + location_status (aggregate across maa's)
    form3 <- str_to_formula(metric, groupvars3)
    
    data_loc <- data_summary
    data_summary <- aggregate(form3, data = data_summary, FUN = mean)
    
    data_summary$N <- aggregate(form3, data = data_loc, FUN = length) %>% dplyr::pull(metric)
    data_summary$SD <- aggregate(form3, data = data_loc, FUN  = sd) %>% dplyr::pull(metric)
    data_summary$SE <- data_summary$SD / sqrt(data_summary$N)
  }
  
  data_summary <- data_summary %>% 
    dplyr::mutate(ymin = !!sym(metric) - SE,
                  ymax = !!sym(metric) + SE)
  return(data_summary)
}

count_unique <- function(x) {
  length(unique(x))
}

str_to_formula <- function(metric, groupvars) {
  # take in the metric to plot (string) the the group variables to plot against (character list)
  # to make a formula: metric ~ groupvar1 + groupvar2 + ...
  as.formula(paste(metric, paste(groupvars, collapse = " + "), sep = " ~ "))
}
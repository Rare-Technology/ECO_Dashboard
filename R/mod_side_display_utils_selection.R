get_display_choices <- function(sel_country, data_full) {
  data_full %>% 
    dplyr::filter(country == sel_country) %>% 
    dplyr::pull('family') %>% 
    unique() %>% 
    as.vector() %>% 
    sort()
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
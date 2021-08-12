get_geo_choices <- function(data_filtered, geo_level) {
  data_filtered %>% 
    dplyr::pull(geo_level) %>% 
    unique() %>% 
    as.vector() %>%
    sort()
}
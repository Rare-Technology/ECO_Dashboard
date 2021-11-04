get_display_choices <- function(sel_maa, data_full) {
  data_full %>% 
    dplyr::filter(ma_name %in% sel_maa) %>% 
    dplyr::pull('family') %>% 
    unique() %>% 
    as.vector() %>% 
    sort()
}

get_year_choices <- function(sel_country, data_full) {
  data_full %>% 
    dplyr::filter(country == sel_country) %>% 
    dplyr::pull(year) %>% 
    unique() %>% 
    sort()
}
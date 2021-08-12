get_display_choices <- function(sel_maa, data_full) {
  data_full %>% 
    dplyr::filter(ma_name %in% sel_maa) %>% 
    dplyr::pull('family') %>% 
    unique() %>% 
    as.vector() %>% 
    sort()
}
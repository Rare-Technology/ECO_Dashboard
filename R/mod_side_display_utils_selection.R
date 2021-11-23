get_display_choices <- function(data_filtered) {
  # data_full %>% 
  #   dplyr::filter(ma_name %in% sel_maa) %>% 
  #   dplyr::pull('family') %>% 
  #   unique() %>% 
  #   as.vector() %>% 
  #   sort()
  data_filtered %>% 
    dplyr::pull("family") %>% 
    unique() %>%
    as.vector() %>% 
    sort()
}
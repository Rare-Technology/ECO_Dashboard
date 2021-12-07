get_geo_choices <- function(dataset,
                            sel_country = NULL,
                            sel_subnational = NULL,
                            sel_local = NULL,
                            sel_maa = NULL,
                            sel_year = NULL,
                            target = NULL) {
  out <- dataset
  if (!is.null(sel_country)) {
    out <- out %>% dplyr::filter(country == sel_country)
  }
  if (!is.null(sel_subnational)) {
    out <- out %>% dplyr::filter(level1_name %in% sel_subnational)
  }
  if (!is.null(sel_local)) {
    out <- out %>% dplyr::filter(level2_name %in% sel_local)
  }
  if (!is.null(sel_maa)) {
    out <- out %>% dplyr::filter(ma_name %in% sel_maa)
  }
  if (!is.null(sel_year)) {
    out <- out %>% dplyr::filter(year == sel_year)
  }
  
  out %>% 
    dplyr::pull(target) %>% 
    unique() %>% 
    as.vector() %>% 
    sort()
}
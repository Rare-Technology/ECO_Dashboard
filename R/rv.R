initialize_rv <- function() {
  reactiveValues(
    current_tab = 'Coral Reefs',
    basemap = providers$Esri.OceanBasemap,
    data_full = fish.surveys,
    data_filtered = fish.surveys %>% dplyr::filter(country == 'HND')
  )
}
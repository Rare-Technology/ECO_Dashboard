initialize_rv <- function() {
  reactiveValues(
    current_tab = 'Coral Reefs',
    basemap = providers$Esri.OceanBasemap,
    # there is a fish family that is blank. currently, selecting that will crash
    # the app with error Warning: Error in aggregate.data.frame: no rows to aggregate
    # for now, we will just take out the ~600 samples that have this nameles family
    data_full = fish.surveys %>% dplyr::filter(family != ""),
    data_filtered = fish.surveys %>% dplyr::filter(country == 'HND')
  )
}
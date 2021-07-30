initialize_rv <- function() {
  reactiveValues(
    current_tab = 'Coral Reefs',
    basemap = providers$Esri.OceanBasemap
  )
}
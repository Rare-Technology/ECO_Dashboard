#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom leaflet leafletOutput renderLeaflet leaflet addProviderTiles addCircleMarkers
mapUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('map_holder'))
  )
}
    
#' map Server Functions
#'
#' @noRd 
mapServer <- function(id, rv){
  ns <- NS(id)
  # points <- cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)

  moduleServer( id, function(input, output, session){
    output$map_holder <- renderUI({
        m <- leaflet(data = rv$data_map) %>%
          addProviderTiles(rv$basemap) %>% 
          addCircleMarkers(lng = rv$data_map$lon,
                           lat = rv$data_map$lat,
                           radius = ~runif(100,1,1),
                           color = 'darkred',
                           fillOpacity = 0.5,
                           popup = popupText(rv$data_map),
                           label = paste("Location :", rv$data_map$location_name)
          )
        output$map <- renderLeaflet(m)
        leafletOutput(ns('map'), width = "100%", height = 800)
      })
    
    ### need a bit of an overhaul on how to pass data through for mapping --
    ### unlike plots, we need all the metrics pass through in one pipeline together!
    observeEvent(rv$data_filtered, {
      if ((rv$current_tab == "Map") & rv$sel_dataset == "Fish") {
        data_filtered <- rv$data_filtered
        cat(nrow(data_filtered))
        coords_filtered <- INIT$COORDS %>%
          dplyr::filter(location_name %in% data_filtered$location_name,
                        year == rv$sel_year)
        rv$data_map <- get_map_data(data_filtered, coords_filtered)
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_map_ui("map_ui_1")
    
## To be copied in the server
# mod_map_server("map_ui_1")

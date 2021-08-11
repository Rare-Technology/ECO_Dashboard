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
        leafletOutput(ns('map'))
      })
    
  })
}
    
## To be copied in the UI
# mod_map_ui("map_ui_1")
    
## To be copied in the server
# mod_map_server("map_ui_1")

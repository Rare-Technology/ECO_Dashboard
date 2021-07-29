#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  rv <- initialize_rv()
  
  mainServer('mainUI', rv)
  
  sidebarServer('sidebarUI')
  sidebarGeoServer('sidebarGeoUI')
  sidebarDisplayServer('sidebarDisplayUI', rv)
  # TODO add other sidebar servers
  
  plotServer('plotUI')
  # mangroveServer('mangroveUI') # have to write from scratch
  mapServer('mapUI')
  # reportServer('reportUI') this isn't implemented correctly on the original dashboard
}

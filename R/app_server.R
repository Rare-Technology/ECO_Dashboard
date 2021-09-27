#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  rv <- initialize_rv(INIT)

  mainServer('mainUI', rv)
  
  sidebarServer('sidebarUI')
  sidebarGeoServer('sidebarGeoUI', rv)
  sidebarDisplayServer('sidebarDisplayUI', rv)
  # TODO add other sidebar servers
  
  startServer('startUI')
  plotServer('plotUI', rv)
  # mangroveServer('mangroveUI') # have to write from scratch
  mapServer('mapUI', rv)
  # reportServer('reportUI') this isn't implemented correctly on the original dashboard
}

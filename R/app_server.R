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
  sidebarYearServer("sidebarYearUI", rv)
  sidebarGeoServer('sidebarGeoUI', rv)
  sidebarDisplayServer('sidebarDisplayUI', rv)
  
  startServer('startUI')
  plotServer('plotUI', rv)
  mangroveServer('mangroveUI', rv)
  mapServer('mapUI', rv)
  # reportServer('reportUI') this isn't implemented correctly on the original dashboard
}

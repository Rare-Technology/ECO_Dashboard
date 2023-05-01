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
  
  startServer('startUI', rv)
  plotServer('plotUI', rv)
  # mapServer('mapUI', rv)
  reportServer('reportUI', rv)
}

#' side UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
sidebarUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarGeoUI('sidebarGeoUI'),
    sidebarDisplayUI('sidebarDisplayUI')
  )
}
    
#' side Server Functions
#'
#' @noRd 
sidebarServer <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_side_ui("side_ui_1")
    
## To be copied in the server
# mod_side_server("side_ui_1")

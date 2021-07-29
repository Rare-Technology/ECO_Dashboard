#' mangrove UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mangroveUI <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mangrove Server Functions
#'
#' @noRd 
mangroveServer <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mangrove_ui("mangrove_ui_1")
    
## To be copied in the server
# mod_mangrove_server("mangrove_ui_1")

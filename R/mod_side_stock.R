#' side_stock UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_side_stock_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' side_stock Server Functions
#'
#' @noRd 
mod_side_stock_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_side_stock_ui("side_stock_ui_1")
    
## To be copied in the server
# mod_side_stock_server("side_stock_ui_1")

#' plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import ggplot2
plotUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('plot_holder'))
  )
}
    
#' plot Server Functions
#'
#' @noRd 
plotServer <- function(id, rv){
  ns <- NS(id)
  moduleServer( id, function(input, output, session){
    output$plot_holder <- renderUI({
      data_filtered <- rv$data_filtered
      p <- switch(rv$sel_metric,
            "Fish Biomass" = plot_biomass(data_filtered)
      )
      output$plot <- renderPlot(p)
      plotOutput(ns('plot'))

    }) # renderUI

  }) #moduleServer
}
    
## To be copied in the UI
# mod_plot_ui("plot_ui_1")
    
## To be copied in the server
# mod_plot_server("plot_ui_1")

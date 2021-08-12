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
      sel_family <- rv$sel_family
      sel_maa <- rv$sel_maa
      y_scale <- rv$sel_yscale
      
      if (is.null(sel_maa)) {
        div(class="warning_message", "No managed access area selected.")
      } else {
        p <- switch(rv$sel_metric,
              "Fish Biomass" = plot_biomass(data_filtered, sel_family),
              "Fish Density" = plot_density(data_filtered, sel_family),
              "Fish Diversity" = plot_diversity(data_filtered, sel_family),
              "Fish Size" = plot_size(data_filtered, sel_family)
        )
        p$facet$params$free$y <- y_scale
        output$plot <- renderPlot(p)
        plotOutput(ns('plot'))
      }
    }) # renderUI

  }) #moduleServer
}
    
## To be copied in the UI
# mod_plot_ui("plot_ui_1")
    
## To be copied in the server
# mod_plot_server("plot_ui_1")

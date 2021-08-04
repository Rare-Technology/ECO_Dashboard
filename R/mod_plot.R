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
      # result <- plot_func()
      # p <- result$p
      
      p <- ggplot2::ggplot(
        data = rv$data_full %>% 
          # dplyr::filter(country == rv$sel_country) %>% 
          # dplyr::filter(level1_name %in% rv$sel_subnational) %>% 
          # dplyr::filter(level2_name %in% rv$sel_local) %>% 
          dplyr::filter(ma_name %in% rv$sel_maa),
        aes(location_status, biomass_kg_ha),
        na.rm = TRUE) +
        geom_bar(aes(fill = location_status), position=position_dodge(),
                 stat = 'identity')
      output$plot <- renderPlot(p)
      plotOutput(ns('plot'))
      
    }) # renderUI

  }) #moduleServer
}
    
## To be copied in the UI
# mod_plot_ui("plot_ui_1")
    
## To be copied in the server
# mod_plot_server("plot_ui_1")

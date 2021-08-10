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
      
      data_aggreg <- rv$data_filtered %>% get_biomass(., 'biomass_kg_ha')
      data_summary <- summarySE(data_aggreg, 'biomass_kg_ha')
      
      p <- ggplot2::ggplot(data=data_summary,
            aes(location_status, biomass_kg_ha),
            na.rm = TRUE) +
        facet_wrap('ma_name') +
        geom_bar(aes(fill = location_status), position=position_dodge(),
                 stat = 'identity') +
        geom_errorbar(aes(ymin=biomass_kg_ha - SE, ymax=biomass_kg_ha + SE),
                      position=position_dodge(), width=0.2, na.rm=TRUE) +
        ggtitle("Mean Fish Biomass")
      output$plot <- renderPlot(p)
      plotOutput(ns('plot'))

    }) # renderUI

  }) #moduleServer
}
    
## To be copied in the UI
# mod_plot_ui("plot_ui_1")
    
## To be copied in the server
# mod_plot_server("plot_ui_1")

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
    theme_update(plot.title = element_text(hjust = 0.5))
    
    output$plot_holder <- renderUI({
      rv$data_filtered <- INIT$DATA_FULL[[rv$sel_dataset]] %>%
        dplyr::filter(ma_name %in% rv$sel_maa,
                      year == rv$sel_year,
                      family %in% rv$sel_family)
      data_filtered <- rv$data_filtered
      sel_family <- rv$sel_family
      sel_maa <- rv$sel_maa
      y_scale <- rv$sel_yscale
      sel_geom <- rv$sel_geom
      sel_year <- rv$sel_year
      
      if (length(sel_maa) == 0) {
        div(class="warning_message", "No managed access area selected.")
      } else {
        p <- switch(rv$sel_metric,
              "Fish Biomass" = plot_biomass(data_filtered, sel_year, sel_family, sel_geom),
              "Fish Density" = plot_density(data_filtered, sel_year, sel_family, sel_geom),
              "Fish Diversity" = plot_diversity(data_filtered, sel_year, sel_family, sel_geom),
              "Fish Size" = plot_size(data_filtered, sel_year, sel_family, sel_geom)
        )
        p$facet$params$free$y <- y_scale
        rv$current_plot <- p
        output$plot <- renderPlot(p, height=600)
        
        ui_out <- list(list(br()))
        ui_out <- append(ui_out, list(downloadButton(ns("downloadPlot"),
                                                     class = "download-button",
                                                     'Download Plot')))
        ui_out <- append(ui_out, list(plotOutput(ns('plot'))))
        ui_out
      }
    })
    
    output$downloadPlot <- downloadHandler(
      filename = function(){paste0("plot_", tolower(gsub(" ", "_", rv$sel_metric)), ".png")},
      content = function(file){
        ggsave(file,plot=rv$current_plot, width = 27, height = 20, units = "cm")
      })
  })
}
    
## To be copied in the UI
# mod_plot_ui("plot_ui_1")
    
## To be copied in the server
# mod_plot_server("plot_ui_1")

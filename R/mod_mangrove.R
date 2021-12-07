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
    uiOutput(ns('plot_holder'))
  )
}
    
#' mangrove Server Functions
#'
#' @noRd 
mangroveServer <- function(id, rv){
  ns <- NS(id)
  moduleServer( id, function(input, output, session){
    theme_update(plot.title = element_text(hjust = 0.5))
    
    output$plot_holder <- renderUI({
      data_mangroves <- rv$data_filtered
      sel_maa <- rv$sel_maa
      y_scale <- rv$sel_yscale
      sel_geom <- rv$sel_geom
      sel_metric <- rv$sel_metric
      
      if (is.null(sel_maa)) {
        div(class="warning_message", "No managed access area selected.")
      } else {
        p <- switch(sel_metric,
                    "Sapling Density" = plot_sapling_tree_density(data_mangroves, sel_geom),
                    "Tree Diversity" = plot_tree_diversity(data_mangroves, sel_geom),
                    "Tree Size" = plot_tree_size(data_mangroves, sel_geom))
        p$facet$params$free$y <- y_scale
        rv$current_plot <- p
        output$plot <- renderPlot(p, height = 600)
        
        ui <- tagList(
          br(),
          downloadButton(ns("downloadPlot"),
           class = "download-button",
           "Download Plot"
          ),
          plotOutput(ns("plot"))
        )
        
        ui
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
# mod_mangrove_ui("mangrove_ui_1")
    
## To be copied in the server
# mod_mangrove_server("mangrove_ui_1")

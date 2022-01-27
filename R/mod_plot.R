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
      sel_dataset <- rv$sel_dataset
      sel_maa <- rv$sel_maa
      facet_maa <- rv$facet_maa
      y_scale <- rv$sel_yscale
      sel_geom <- rv$sel_geom
      sel_year <- rv$sel_year
      
      if (sel_dataset == "Fish") {
        sel_family <- rv$sel_family
        rv$data_filtered <- INIT$DATA_FULL[[sel_dataset]] %>%
          dplyr::filter(ma_name %in% sel_maa,
                        # year == sel_year,
                        family %in% sel_family)
      } else {
        rv$data_filtered <- INIT$DATA_FULL[[sel_dataset]] %>% 
          dplyr::filter(ma_name %in% sel_maa)#,
                        # year == sel_year)
      }
      data_filtered <- rv$data_filtered
      
      if (length(sel_maa) == 0) {
        div(class="warning_message", tr(rv, "No managed access area selected."))
      } else {
        p <- switch(rv$sel_metric,
              "Fish biomass" = plot_fish_biomass(data_filtered, sel_geom, facet_maa),
              "Fish density" = plot_fish_density(data_filtered, sel_geom, facet_maa),
              "Fish diversity" = plot_fish_diversity(data_filtered, sel_geom, facet_maa),
              "Fish size" = plot_fish_size(data_filtered, sel_geom, facet_maa),
              "Sapling density" = plot_sapling_tree_density(data_filtered, sel_geom, facet_maa),
              "Tree diversity" = plot_tree_diversity(data_filtered, sel_geom, facet_maa),
              "Tree size" = plot_tree_size(data_filtered, sel_geom, facet_maa),
              "Benthic cover" = plot_reef_cover(data_filtered, sel_geom, facet_maa),
              "Benthic diversity" = plot_reef_diversity(data_filtered, sel_geom, facet_maa),
              "Seagrass cover" = plot_seagrass_cover(data_filtered, sel_geom, facet_maa),
              "Seagrass diversity" = plot_seagrass_diversity(data_filtered, sel_geom, facet_maa),
              "Seagrass height" = plot_seagrass_height(data_filtered, sel_geom, facet_maa)
        )
        if (facet_maa) {
          p$plot <- p$plot + facet_wrap('ma_name')
        }
        p$plot$facet$params$free$y <- y_scale
        rv$current_plot <- p$plot
        rv$current_plot_data <- p$data
        output$plot <- renderPlot(p$plot, height=600)
        
        ui_out <- list(list(br()))
        ui_out <- append(ui_out, list(downloadButton(ns("downloadPlot"),
                                                     class = "download-button",
                                                     tr(rv, 'Download plot'))))
        ui_out <- append(ui_out, list(plotOutput(ns('plot'))))
        ui_out
      }
    })
    
    output$downloadPlot <- downloadHandler(
      filename = function() {paste0("plot_", tolower(gsub(" ", "_", rv$sel_metric)), ".zip")},
      content = function(file) {
        wd <- getwd()
        setwd(tempdir())
        
        meta_name <- 'filters.txt'
        data_name <- 'data.csv'
        plot_name <- paste0("plot_", tolower(gsub(" ", "_", rv$sel_metric)), ".png")
        
        filters_text <- display_filters(rv)
        write(filters_text, meta_name)
        write.csv(rv$current_plot_data, data_name, row.names = FALSE)
        ggsave(plot_name, plot = rv$current_plot, width = 27, height = 20, units = "cm")
        
        fs = c(meta_name, data_name, plot_name)
        zip(zipfile = file, files = fs)
        
        setwd(wd)
      },
      contentType = 'application/zip'
    )
  })
}
    
## To be copied in the UI
# mod_plot_ui("plot_ui_1")
    
## To be copied in the server
# mod_plot_server("plot_ui_1")

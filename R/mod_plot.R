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
  uiOutput(ns('plot_holder'))
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
      sel_metric <- rv$sel_metric
      sel_maa <- rv$sel_maa
      facet_maa <- rv$facet_maa
      y_scale <- rv$sel_yscale
      sel_geom <- rv$sel_geom

      if (length(sel_maa) == 0) {
        return(div(class="warning_message", tr(rv, "No managed access area selected.")))
      }
      
      if (sel_dataset == "Fish") {
        sel_family <- rv$sel_family
        if(length(sel_family) == 0) {
          return(div(class="warning_message", tr(rv, "No fish families selected.")))
        }
        rv$data_filtered <- INIT$DATA_FULL[['Fish']] %>%
          dplyr::filter(ma_name %in% sel_maa,
                        family %in% sel_family)
      } else {
        rv$data_filtered <- INIT$DATA_FULL[[sel_dataset]] %>% 
          dplyr::filter(ma_name %in% sel_maa)
      }
      data_filtered <- rv$data_filtered
      
      p <- try(PLOT_FUN(sel_metric, data_filtered, sel_geom, facet_maa))
      
      if (class(p) == "try-error") {
        return(div(class="warning_message", tr(rv, PLOT_ERROR)))
      }
      # Facet if necessary and adjust arrangement, fix y-scale, get plot height for rendering
      clean_plot_out <- clean_plot(p, facet_maa, y_scale, sel_metric, sel_maa)
      p <- clean_plot_out$p
      plot_height <- clean_plot_out$plot_height
      rv$current_plot <- p$plot
      rv$current_plot_height <- plot_height
      rv$current_plot_data <- p$data
      output$plot <- renderPlot(p$plot, height = plot_height, width=1000) 
  
      ui_out <- list(list(br()))
      ui_out <- append(ui_out, list(
        div(id = "download-button-container",
          downloadButton(ns("downloadPlot"), tr(rv, 'Download plot'))
        )
      ))

      
      ui_out <- append(ui_out, list(
        plotOutput(ns('plot'))
      ))
      return(ui_out)
    })
    
    output$downloadPlot <- downloadHandler(
      filename = function() {paste0("plot_", tolower(gsub(" ", "_", rv$sel_metric)), ".zip")},
      content = function(file) {
        # Write out file paths
        tmpdir <- tempdir()
        meta_name <- file.path(tmpdir, 'filters.txt')
        data_name <- file.path(tmpdir, 'data.csv')
        plot_name <- file.path(tmpdir, paste0("plot_", tolower(gsub(" ", "_", rv$sel_metric)), ".png"))
        
        # Save metadata
        filters_text <- display_filters(rv)
        write(filters_text, meta_name)
        
        # Save data
        write.csv(rv$current_plot_data, data_name, row.names = FALSE)
        
        # Save plots
        plot_height <- rv$current_plot_height
        plot_height <- ifelse(plot_height == "auto", 500, plot_height)
        # Maybe adjust this at some point so plots aren't so large, esp. when
        # plotting only one or two facet rows or one aggregate plot.
        png(plot_name, width=500*300/72, height=plot_height*300/72, res=300)
        print(rv$current_plot) # don't delete this !! This is how the graphic data is saved
        dev.off()
        
        # Set up zip archive
        fs = c(meta_name, data_name, plot_name)
        zip(zipfile=file, files=fs, flags="-j")
      },
      contentType = 'application/zip'
    )
  })
}

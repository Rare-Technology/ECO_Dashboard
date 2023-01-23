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
      
      p <- try(
        switch(sel_metric,
          "Fish biomass" = plot_fish_biomass(data_filtered, sel_geom, facet_maa),
          "Fish density" = plot_fish_density(data_filtered, sel_geom, facet_maa),
          "Fish diversity" = plot_fish_diversity(data_filtered, sel_geom, facet_maa),
          "Fish size" = plot_fish_size(data_filtered, sel_geom, facet_maa),
          "Sapling density" = plot_sapling_tree_density(data_filtered, sel_geom, facet_maa),
          "Tree diversity" = plot_tree_diversity(data_filtered, sel_geom, facet_maa),
          "Tree size" = plot_tree_size(data_filtered, sel_geom, facet_maa),
          "Coral reef cover" = plot_reef_cover(data_filtered, sel_geom, facet_maa),
          "Coral reef diversity" = plot_reef_diversity(data_filtered, sel_geom, facet_maa),
          "Seagrass cover" = plot_seagrass_cover(data_filtered, sel_geom, facet_maa),
          "Seagrass diversity" = plot_seagrass_diversity(data_filtered, sel_geom, facet_maa),
          "Seagrass height" = plot_seagrass_height(data_filtered, sel_geom, facet_maa),
          "Oyster density" = plot_oyster_density(data_filtered, sel_geom, facet_maa),
          "Oyster size" = plot_oyster_size(data_filtered, sel_geom, facet_maa),
          "Crab density" = plot_crab_density(data_filtered, sel_geom, facet_maa),
          "Crab size" = plot_crab_size(data_filtered, sel_geom, facet_maa)
        )
      )
      
      if (class(p) == "try-error") {
        return(div(class="warning_message", tr(rv, PLOT_ERROR)))
      } else {
        if (facet_maa) {
          if (sel_metric == "Coral reef cover") {
            p$plot <- p$plot + facet_grid(rows = vars(ma_name), cols = vars(location_status))
            plot_height <- get_plot_height(2*length(rv$sel_maa))
          } else {
            p$plot <- p$plot + facet_wrap('ma_name', ncol = 2)
            plot_height <- get_plot_height(length(rv$sel_maa))
          }
        } else {
          if (sel_metric == "Coral reef cover") {
            p$plot <- p$plot + facet_wrap("location_status")
          }
          plot_height <- 'auto'
        }
        p$plot$facet$params$free$y <- !y_scale
        p$plot <- p$plot + ggplot2::labs(caption = WATERMARK_LABEL)
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
          plotOutput(ns('plot')) #%>% 
            # tagAppendAttributes(style = "margin: 0 auto;")
        ))
        return(ui_out)
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
        plot_height <- rv$current_plot_height
        plot_height <- ifelse(plot_height == "auto", 500, plot_height)
        # Maybe adjust this at some point so plots aren't so large, esp. when
        # plotting only one or two facet rows or one aggregate plot.
        png(plot_name, width=500*300/72, height=plot_height*300/72, res=300)
        dev.off()
        
        fs = c(meta_name, data_name, plot_name)
        zip(zipfile = file, files = fs)
        
        setwd(wd)
      },
      contentType = 'application/zip'
    )
  })
}

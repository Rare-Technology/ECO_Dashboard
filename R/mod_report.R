#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom rmarkdown render
#' @importFrom writexl write_xlsx
reportUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("report_page"))
}
    
#' report Server Functions
#'
#' @noRd 
reportServer <- function(id, rv){
  ns <- NS(id)
  moduleServer( id, function(input, output, session){
    output$report_page <- renderUI({
      ui_out <- list()
      
      ui_out <- append(ui_out, list(checkboxGroupInput(
        ns("report_metrics"),
        strong(tr(rv, "Ecological metrics")),
        choices = unname(unlist(INIT$METRICS)),
        selected = NULL,
        inline = FALSE
      )))
      
      ui_out <- append(ui_out, list(downloadButton(
        ns("downloadReport"), tr(rv, 'Download report')
      )))
      
      return(ui_out)
    })
    
    output$downloadReport <- downloadHandler(
      filename = "Ecological_monitoring_summary_report.zip",
      content = function(file) {
        # First, process data and plots required for report
        report_metrics <- input$report_metrics
        sel_maa <- rv$sel_maa
        facet_maa <- rv$facet_maa
        y_scale <- rv$sel_yscale
        sel_geom <- rv$sel_geom
        data_filtered <- list()
        p <- list()
        
        # Treat fish plots separately since they require family input
        if (any(INIT$METRICS$Fish %in% report_metrics)) {
          sel_family <- rv$sel_family
          data_filtered[["Fish"]] <- INIT$DATA_FULL[['Fish']] %>%
            dplyr::filter(
              ma_name %in% sel_maa,
              family %in% sel_family
            )
          for (metric in INIT$METRICS$Fish) {
            if (metric %in% report_metrics) {
              cat("hello")
              p$Fish[[metric]] <- PLOT_FUN(metric, data_filtered$Fish, sel_geom, facet_maa)
              cat("world")
              p$Fish[[metric]] <- clean_plot(p$Fish[[metric]], facet_maa, y_scale, metric, sel_maa)$p
            }
          }
        }
        # Now plot everything else
        for (survey in setdiff(names(INIT$METRICS), "Fish")) {
          for (metric in intersect(INIT$METRICS[[survey]], report_metrics)) {
            data_filtered[[survey]] <- INIT$DATA_FULL[[survey]] %>% 
              dplyr::filter(ma_name %in% sel_maa)
            cat("cat")
            p[[survey]][[metric]] <- PLOT_FUN(metric, data_filtered[[survey]], sel_geom, facet_maa)
            cat("dog")
            p[[survey]][[metric]] <- clean_plot(p[[survey]][[metric]], facet_maa, y_scale, metric, sel_maa)$p
          }
        }
        
        # Export word doc
        tmpdir <- tempdir()
        report_path <- file.path(tmpdir, "Ecological_monitoring_summary_report.doc")
        withProgress(message = 'Generating report, please wait, this could take up to 20 seconds', {
          render(input="R/report-template.Rmd", output_file=report_path, output_format='word_document')
        })
        
        # Export excel
        data_path <- file.path(tmpdir, "report_data.xlsx")
        report_data <- list()
        for (survey in names(p)) {
          for (metric in names(p[[survey]])) {
            report_data[[metric]] <- p[[survey]][[metric]]$data
          }
        }
        write_xlsx(report_data, data_path)
          
        zip(zipfile = file, files=c(report_path, data_path), flags="-j")
      }
    )
  })
}

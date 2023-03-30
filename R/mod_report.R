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
        strong("Ecological metrics"),
        choices = INIT$METRICS$Fish,
        selected = NULL,
        inline = FALSE
      )))
      
      ui_out <- append(ui_out, list(downloadButton(
        ns("downloadReport"), tr(rv, 'Download report')
      )))
      
      return(ui_out)
    })
    
    output$downloadReport <- downloadHandler(
      filename = 'Ecological_monitoring_summary_report.doc',
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("R/report-template.Rmd", tempReport, overwrite = TRUE)
        withProgress(message = 'Generating report, please wait, this could take up to 20 seconds', {
          render(tempReport, output_file = file, output_format = 'word_document')
        })
      }
    )
  })
}

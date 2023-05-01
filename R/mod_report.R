#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyjs enable disable
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
      if (length(rv$sel_maa) == 0) {
        return(div(class="warning_message", tr(rv, "No managed access area selected.")))
      }
      
      ui_out <- list()
      
      ui_out <- append(ui_out, list(checkboxGroupInput(
        ns("report_metrics"),
        strong(tr(rv, "Ecological metrics")),
        choices = unname(unlist(INIT$METRICS)),
        selected = NULL,
        inline = FALSE
      )))
      
      ui_out <- append(
        ui_out,
        list(
          downloadButton(ns("downloadReport"), tr(rv, 'Download report')) %>% 
            tagAppendAttributes(disabled=TRUE)
        )
      )
      
      ui_out <- append(
        ui_out,
        list(div(id="download-report-link-div", style="visibility:hidden;"))
      )
      
      return(ui_out)
    })
    
    observeEvent(rv$sel_maa, {
        # Based on selected maa's, show checkboxes only for metrics from surveys that contain at least
        # one of the selected maa's.
        survey_choices <- names(INIT$METRICS)
        metric_choices <- c()
        for (survey in survey_choices) {
          survey_maa_list <- unique(INIT$DATA_FULL[[survey]]$ma_name)
          if (any(rv$sel_maa %in% survey_maa_list)) {
            metric_choices <- c(metric_choices, INIT$METRICS[[survey]])
          }
        }
        
        updateCheckboxGroupInput(
          session,
          "report_metrics",
          choices = metric_choices
        )
      },
      ignoreInit=TRUE
    )
    
    observeEvent(input$report_metrics, {
      if (length(input$report_metrics) == 0) {
        disable(ns("downloadReport"), asis=TRUE)
      } else {
        enable(ns("downloadReport"), asis=TRUE)
      }
    }, ignoreNULL=FALSE)
    
    observeEvent(input$downloadReport, {
      
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
        error_metrics <- c()
        
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
              p_attempt <- try(PLOT_FUN(metric, data_filtered$Fish, sel_geom, facet_maa))
              if (class(p_attempt) == "try-error") {
                error_metrics <- c(error_metrics, metric)
              } else {
                p$Fish[[metric]] <- clean_plot(p_attempt, facet_maa, y_scale, metric, sel_maa)$p
              }
            }
          }
        }
        # Now plot everything else
        for (survey in setdiff(names(INIT$METRICS), "Fish")) {
          for (metric in intersect(INIT$METRICS[[survey]], report_metrics)) {
            data_filtered[[survey]] <- INIT$DATA_FULL[[survey]] %>% 
              dplyr::filter(ma_name %in% sel_maa)
            p_attempt <- try(PLOT_FUN(metric, data_filtered[[survey]], sel_geom, facet_maa))
            if (class(p_attempt) == "try-error") {
              error_metrics <- c(error_metrics, metric)
            } else {
              p[[survey]][[metric]] <- clean_plot(p_attempt, facet_maa, y_scale, metric, sel_maa)$p
            }
          }
        }
        
        report_metrics <- setdiff(report_metrics, error_metrics)

        if(length(error_metrics) > 0) {
          msg <- paste("Something happened and a plot could not be made for these metrics:", paste(error_metrics, sep=", "))
          showNotification(msg, duration=30, type="warning")
        }

        # Export word doc
        tmpdir <- tempdir()
        report_path <- file.path(tmpdir, "Ecological_monitoring_summary_report.docx")
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
      },
      contentType = 'application/zip'
    )
  })
}

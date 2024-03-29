#' side_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput multiInput switchInput radioGroupButtons updatePickerInput materialSwitch
#' @importFrom leaflet providers
sidebarDisplayUI <- function(id){
  ns <- NS(id)
  uiOutput(ns('display'))
}
    
#' side_plot Server Functions
#'
#' @noRd 
sidebarDisplayServer <- function(id, rv){
  ns <- NS(id)
  moduleServer( id, function(input, output, session){
    output$display <- renderUI({
      sel_dataset <- rv$sel_dataset
      current_tab <- rv$current_tab
      
      ui <- tagList(br(), div(class="sidetitle", tr(rv, "Plot")))
      if (current_tab == tr(rv, "Visualize data")) {
        ui <- tagList(
          ui,
          selectInput(
            ns('sel_metric'),
            tr(rv, 'Metric'),
            choices = INIT$METRICS[["Fish"]]
          )
        )
      }
      ui <- tagList(
        ui,
        radioButtons(
          ns('sel_geom'),
          tr(rv, 'Plot Type'),
          choices = c('Bar plots', 'Distribution plots')
        ),
        materialSwitch(
          ns('facet_maa'),
          label = tr(rv,"Group by MA"), 
          value = TRUE,
          status = "primary"
        ),
        materialSwitch(
          ns("sel_yscale"),
          tr(rv, "Fixed Y-axis"),
          value = TRUE,
          width = "100%",
          status = "primary"
        )
      )
      if (sel_dataset == "Fish" | current_tab == tr(rv, "Report")) {
        ui <- tagList(
          ui,
          pickerInput(
            ns('sel_family'),
            tr(rv, 'Family'),
            choices = INIT$FAMILY$CHOICES,
            selected = INIT$FAMILY$SELECTED,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `selected-text-format` = "count > 3",
              `count-selected-text` = paste("{0}", tr(rv, "items selected")),
              `none-selected-text` = tr(rv, "Nothing selected")
            ),
            multiple = TRUE
          )
        )
      }
      
      ui
    })
    
    observeEvent(input$basemap, {
      rv$basemap <- input$basemap
    })
    
    observeEvent(rv$sel_dataset, {
      metrics <- INIT$METRICS[[rv$sel_dataset]]
      rv$sel_metric <- metrics[1]
      updateSelectInput(
        session,
        "sel_metric",
        choices = metrics,
        selected = metrics[1]
      )
    }, ignoreInit = TRUE)
    
    observeEvent(input$sel_metric, {
      rv$sel_metric <- input$sel_metric
      updateSelectInput(
        session,
        'sel_metric',
        selected = input$sel_metric
      )
    }, ignoreInit = TRUE)
    
    observeEvent(input$sel_geom, {
      rv$sel_geom <- input$sel_geom
    })
    
    observeEvent(input$facet_maa, {
      rv$facet_maa <- input$facet_maa
    })
    
    observeEvent(input$sel_yscale, {
      rv$sel_yscale <- as.logical(input$sel_yscale)
    })
    
    observeEvent(rv$sel_maa, {
      if (rv$sel_dataset == "Fish") {
        family_choices <- get_geo_choices(INIT$DATA_FULL[["Fish"]],
          sel_country = rv$sel_country,
          sel_subnational = rv$sel_subnational,
          sel_local = rv$sel_local,
          sel_maa = rv$sel_maa,
          target = "family")
        rv$sel_family <- family_choices
        updatePickerInput(
          session,
          "sel_family",
          choices = family_choices,
          selected = family_choices
        )
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    observeEvent(input$sel_family, {
      if (!setequal(rv$sel_family, input$sel_family)) {
        rv$sel_family <- input$sel_family
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  })
}
    
## To be copied in the UI
# mod_side_plot_ui("side_plot_ui_1")
    
## To be copied in the server
# mod_side_plot_server("side_plot_ui_1")

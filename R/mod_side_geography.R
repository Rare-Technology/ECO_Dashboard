#' side_geography UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput updatePickerInput
sidebarGeoUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("geo"))
}
    
#' side_geography Server Functions
#'
#' @noRd 
sidebarGeoServer <- function(id, rv){
  ns <- NS(id)
  
  moduleServer( id, function(input, output, session){
    output$geo <- renderUI({
      ui <- tagList(
        div(class = "sidetitle", tr(rv, "Data")),
        selectInput(
          ns("sel_dataset"),
          tr(rv, "Data source"),
          choices = INIT$DATASET$CHOICES,
          selected = INIT$DATASET$SELECTED
        ),
        div(class="sidetitle", tr(rv, "Geography")),
        selectInput(
          ns('sel_country'),
          tr(rv, 'Country'),
          choices = INIT$COUNTRY$CHOICES,
          selected = INIT$COUNTRY$SELECTED
        ),
        pickerInput(
          ns('sel_subnational'),
          tr(rv, 'Subnational government'),
          choices = INIT$SUBNATIONAL$CHOICES,
          selected = INIT$SUBNATIONA$SELECTED,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = "count > 2",
            `count-selected-text` = paste("{0}", tr(rv, "items selected")),
            `none-selected-text` = tr(rv, "Nothing selected")
          )
        ),
        pickerInput(
          ns('sel_local'),
          tr(rv, 'Local government'),
          choices = INIT$LOCAL$CHOICES,
          selected = INIT$LOCAL$SELECTED,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = "count > 2",
            `count-selected-text` = paste("{0}", tr(rv, "items selected")),
            `none-selected-text` = tr(rv, "Nothing selected")
            
          )
        ),
        pickerInput(
          ns('sel_maa'),
          tr(rv, 'Managed Access Area'),
          choices = INIT$MAA$CHOICES,
          selected = INIT$MAA$SELECTED,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = "count > 3",
            `count-selected-text` = paste("{0}", tr(rv, "items selected")),
            `none-selected-text` = tr(rv, "Nothing selected")
          )
        )
      )
      
      ui
    })
    
    observeEvent(input$sel_dataset, {
      if(rv$sel_dataset != input$sel_dataset){
        rv$sel_dataset <- input$sel_dataset
        country_choices <- get_geo_choices(INIT$DATA_FULL[[rv$sel_dataset]],
                                           target = "country")
        rv$sel_country <- country_choices[1]
        updateSelectInput(
          session,
          "sel_country",
          choices = country_choices,
          selected = country_choices[1]
        )
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    observeEvent(input$sel_country, {
      # update rv$sel_country only as a result of changing input$sel_country,
      # rather than as a result of updating input$dataset
      # this prevents unnecessary computation; without this check, every time we
      # change input$dataset, rv$sel_country will update twice
      # for this same reason, this check is down for all down stream selectors
      if (rv$sel_country != input$sel_country) {
        rv$sel_country <- input$sel_country
      }
      subnational_choices <- get_geo_choices(INIT$DATA_FULL[[rv$sel_dataset]],
                                             sel_country = input$sel_country,
                                             target = 'level1_name')
      rv$sel_subnational <- subnational_choices
      updatePickerInput(
        session,
        'sel_subnational',
        choices = subnational_choices,
        selected = subnational_choices
      )
    }, ignoreInit = TRUE)
    
    observeEvent(input$sel_subnational, {
      if (!setequal(rv$sel_subnational, input$sel_subnational)) {
        rv$sel_subnational <- input$sel_subnational
      }
      local_choices <- get_geo_choices(INIT$DATA_FULL[[rv$sel_dataset]],
                                       sel_country = rv$sel_country,
                                       sel_subnational = rv$sel_subnational,
                                       target = 'level2_name')
      rv$sel_local <- local_choices
      updatePickerInput(
        session,
        'sel_local',
        choices = local_choices,
        selected = local_choices
      )
    }, ignoreInit=TRUE)
    
    observeEvent(input$sel_local, {
      if (!setequal(rv$sel_local, input$sel_local)) {
        rv$sel_local <- input$sel_local
      }
      maa_choices <- get_geo_choices(INIT$DATA_FULL[[rv$sel_dataset]],
                                     sel_country = rv$sel_country,
                                     sel_subnational = rv$sel_subnational,
                                     sel_local = rv$sel_local,
                                     target = 'ma_name')
      rv$sel_maa <- NULL
      updatePickerInput(
        session,
        'sel_maa',
        choices = maa_choices,
        selected = NULL
      )
    }, ignoreInit=TRUE)
    
    observeEvent(input$sel_maa, {
      if (!setequal(rv$sel_maa, input$sel_maa)) {
        rv$sel_maa <- input$sel_maa
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  })
}
    
## To be copied in the UI
# mod_side_geography_ui("side_geography_ui_1")
    
## To be copied in the server
# mod_side_geography_server("side_geography_ui_1")

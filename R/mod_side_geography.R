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
sidebarGeoUI <- function(id, INIT){
  ns <- NS(id)
  tagList(
    selectInput(
      ns('sel_country'),
      'Country',
      choices = INIT$COUNTRY_CHOICES
    ),
    pickerInput(
      ns('sel_subnational'),
      'Subnational government',
      choices = INIT$SUBNATIONAL_CHOICES,
      selected = INIT$SUBNATIONAL_CHOICES,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2"
      )
    ),
    pickerInput(
      ns('sel_local'),
      'Local government',
      choices = INIT$LOCAL_CHOICES,
      selected = INIT$LOCAL_CHOICES,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2"
      )
    ),
    pickerInput(
      ns('sel_maa'),
      'Managed access area',
      choices = INIT$MAA_CHOICES,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2"
      )
    ) # pickerInput
  ) # tagList
} #sidebarGeoUI
    
#' side_geography Server Functions
#'
#' @noRd 
sidebarGeoServer <- function(id, rv){
  ns <- NS(id)
  moduleServer( id, function(input, output, session){
    observeEvent(input$sel_country, {
      rv$sel_country <- input$sel_country
      rv$data_filtered <- rv$data_full %>%
        dplyr::filter(country == input$sel_country)
      rv$subnational_choices <- get_choices(rv$data_filtered, 'level1_name')
      updatePickerInput(
        session,
        'sel_subnational',
        choices = rv$subnational_choices,
        selected = rv$subnational_choices
      )
    },
    ignoreInit = TRUE
    )
    observeEvent(input$sel_subnational, {
      rv$sel_subnational <- input$sel_subnational
      rv$data_filtered <- rv$data_full %>%
        dplyr::filter(level1_name %in% input$sel_subnational)
      rv$local_choices <- get_choices(rv$data_filtered, 'level2_name')
      updatePickerInput(
        session,
        'sel_local',
        choices = rv$local_choices,
        selected = rv$local_choices
      )
    },
    ignoreInit=TRUE
    )
    observeEvent(input$sel_local, {
      rv$sel_local <- input$sel_local
      rv$data_filtered <- rv$data_full %>%
        dplyr::filter(level2_name %in% input$sel_local)
      rv$maa_choices <- get_choices(rv$data_filtered, 'ma_name')
      updatePickerInput(
        session,
        'sel_maa',
        choices = rv$maa_choices,
        selected = NULL
      )
    },
    ignoreInit=TRUE
    )
    observeEvent(input$sel_maa, {
      rv$sel_maa <- input$sel_maa
      rv$data_filtered <- rv$data_full %>%
        dplyr::filter(country == input$sel_country,
                      level1_name %in% input$sel_subnational,
                      level2_name %in% input$sel_local,
                      ma_name %in% input$sel_maa)
      rv$data_map <- aggregate(cbind(lat, lon) ~
                                 country +
                                 level1_name +
                                 level2_name +
                                 ma_name +
                                 location_name,
                               data = rv$data_filtered,
                               FUN = mean, na.rm = TRUE)
    })
  })
}
    
## To be copied in the UI
# mod_side_geography_ui("side_geography_ui_1")
    
## To be copied in the server
# mod_side_geography_server("side_geography_ui_1")

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
  tagList(
    selectInput(
      ns('sel_country'),
      'Country',
      choices = fish.surveys$country %>% unique() 
    ),
    pickerInput(
      ns('sel_subnational'),
      'Subnational government',
      choices = fish.surveys$level1_name %>%
        unique() %>%
        as.vector(),
      selected = fish.surveys$level1_name %>% 
        unique() %>% 
        as.vector(),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2"
      )
    ),
    pickerInput(
      ns('sel_local'),
      'Local government',
      choices = fish.surveys$level2_name %>% 
        unique() %>% 
        as.vector(),
      selected = fish.surveys$level2_name %>% 
        unique() %>% 
        as.vector(),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2"
      )
    ),
    pickerInput(
      ns('sel_maa'),
      'Managed access area',
      choices = fish.surveys$ma_name %>% 
        unique() %>% 
        as.vector(),
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
      rv$subnational_choices <- rv$data_filtered %>% 
        dplyr::pull(level1_name) %>%
        unique() %>% 
        as.vector() %>% 
        sort()
      updatePickerInput(
        session,
        'sel_subnational',
        choices = rv$subnational_choices,
        selected = rv$subnational_choices
      )
    },
    ignoreInit = TRUE
    )
    observeEvent(input$sel_maa, {
      rv$data_filtered <- rv$data_full %>%
        dplyr::filter(
          country %in% input$sel_country,
          level1_name %in% input$sel_subnational,
          level2_name %in% input$sel_local,
          ma_name %in% input$sel_maa
        )
    })
  })
}
    
## To be copied in the UI
# mod_side_geography_ui("side_geography_ui_1")
    
## To be copied in the server
# mod_side_geography_server("side_geography_ui_1")

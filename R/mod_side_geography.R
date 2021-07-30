#' side_geography UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput
sidebarGeoUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      ns('sel_country'),
      'Country',
      choices = fish.surveys %>%
        dplyr::select(country) %>%
        unique()
    ),
    pickerInput(
      ns('sel_subnational'),
      'Subnational government',
      choices = LETTERS,
      selected = LETTERS,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2"
      )
    ),
    pickerInput(
      ns('sel_local'),
      'Local government',
      choices = letters,
      selected = letters,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2"
      )
    ),
    pickerInput(
      ns('sel_maa'),
      'Managed access area',
      choices = month.abb,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2"
      )
    )
  )
}
    
#' side_geography Server Functions
#'
#' @noRd 
sidebarGeoServer <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_side_geography_ui("side_geography_ui_1")
    
## To be copied in the server
# mod_side_geography_server("side_geography_ui_1")

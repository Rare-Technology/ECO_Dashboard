#' side_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput multiInput
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
      ui <- NULL #list()
      current_tab <- rv$current_tab
      
      if (rv$current_tab == 'Coral Reefs') {
        ui <- tagList(
          selectInput(ns('sel_metric'),
                      'Metric',
                      choices = c('Fish Biomass', 'Fish Density', 'Fish Diversity',
                                  'Fish Size', 'Habitat Cover', 'Habitat Diversity')
          ),
          radioButtons(ns('sel_plot_type'),
                       'Plot Type',
                       choices = c('Bar plots', 'Range plots')
          ),
          radioButtons(ns('sel_yaxis'),
                       'Y-axis',
                       choices = c('Free', 'Fixed')
          ),
          pickerInput(ns('sel_family'),
                      'Fish family',
                      choices = LETTERS,
                      options = list(
                        `actions-box` = TRUE,
                        `selected-text-format` = "count > 3"
                      ),
                      multiple = TRUE
          ),
          pickerInput(ns('sel_species'),
                      'Fish species',
                      choices = LETTERS,
                      options = list(
                        `actions-box` = TRUE,
                        `selected-text-format` = "count > 2"
                      ),
                      multiple = TRUE
          )
        ) # tagList
      } # if
      
      if (current_tab == 'Map') {
        
        ui <- div(
          selectInput(ns('basemap'),
            'Select Basemap',
            choices= c("Gray Canvas basemap" = providers$Esri.WorldGrayCanvas,
                      "National Geographic basemap" = providers$Esri.NatGeoWorldMap,
                      "Ocean basemap"= providers$Esri.OceanBasemap,
                      "Satellite basemap"= providers$Esri.WorldImagery,
                      "World Topo basemap" = providers$Esri.WorldTopoMap),
            selected = rv$basemap
          ) # selectInput
        ) # div
      } # if
      ui
    }) # renderUI
    
    observeEvent(input$basemap, {
      rv$basemap <- input$basemap
    })
  }) # modServer
} # server
    
## To be copied in the UI
# mod_side_plot_ui("side_plot_ui_1")
    
## To be copied in the server
# mod_side_plot_server("side_plot_ui_1")

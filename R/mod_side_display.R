#' side_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput multiInput switchInput
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
      
      if (current_tab == 'Coral Reefs') {
        ui <- tagList(
          div(class="sidetitle", "Plotting"),
          selectInput(ns('sel_metric'),
                      'Metric',
                      # todo: habitat diversity, habitat cover
                      # habitat diversity only worked for IDN in Abel's code
                      # habitat cover didn't work at all
                      choices = c('Fish Biomass', 'Fish Density', 'Fish Diversity',
                                  'Fish Size')
          ),
          radioButtons(ns('sel_geom'),
                       'Plot Type',
                       choices = c('Bar plots', 'Distribution plots')
          ),
          radioButtons(ns('sel_yscale'),
                       'Y-axis',
                       choices = c('Free'=TRUE,
                                   'Fixed'=FALSE),
                       selected = TRUE
          ),
          pickerInput(ns('sel_family'),
                      'Fish family',
                      choices = get_display_choices(rv$sel_maa, rv$data_full$fish),
                      selected = get_display_choices(rv$sel_maa, rv$data_full$fish),
                      options = list(
                        `actions-box` = TRUE,
                        `selected-text-format` = "count > 3"
                      ),
                      multiple = TRUE
          ),
          # switchInput(ns("sel_date"),
          #             "Plot dates")
          # don't need this right now
          # pickerInput(ns('sel_species'),
          #             'Fish species',
          #             choices = LETTERS,
          #             options = list(
          #               `actions-box` = TRUE,
          #               `selected-text-format` = "count > 2"
          #             ),
          #             multiple = TRUE
          # )
        ) # tagList
      } else if(current_tab == "Mangrove Forests") {
        ui <- tagList(
          div(class = "sidetitle", "Plotting"),
          selectInput(ns("sel_metric"),
                      "Metric",
                      choices = c("Tree Size", "Tree Diversity", "Tree Density")
          ),
          radioButtons(ns('sel_geom'),
                       'Plot Type',
                       choices = c('Bar plots', 'Distribution plots')
          ),
          radioButtons(ns('sel_yscale'),
                       'Y-axis',
                       choices = c('Free'=TRUE,
                                   'Fixed'=FALSE),
                       selected = TRUE
          )
        )
      }
      
      if (current_tab == 'Map') {
        
        ui <- tagList(
          div(class="sidetitle", "Map"),
          selectInput(ns('basemap'),
            'Select Basemap',
            choices= c("Gray Canvas basemap" = providers$Esri.WorldGrayCanvas,
                      "National Geographic basemap" = providers$Esri.NatGeoWorldMap,
                      "Ocean basemap"= providers$Esri.OceanBasemap,
                      "Satellite basemap"= providers$Esri.WorldImagery,
                      "World Topo basemap" = providers$Esri.WorldTopoMap),
            selected = rv$basemap
          )
        )
      }
      
      ui
    })
    
    observeEvent(input$basemap, {
      rv$basemap <- input$basemap
    })
    
    observeEvent(rv$sel_maa, {
      updatePickerInput(
        session,
        'sel_family',
        choices = get_display_choices(rv$sel_maa, rv$data_full$fish),
        selected = get_display_choices(rv$sel_maa, rv$data_full$fish)
      )
    })
    
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
    
    observeEvent(input$sel_yscale, {
      rv$sel_yscale <- as.logical(input$sel_yscale)
    })
    
    observeEvent(input$sel_family, {
      rv$sel_family <- input$sel_family
      # rv$data_filtered <- rv$data_full %>%
      #   dplyr::filter(ma_name %in% rv$sel_maa,
      #                 family %in% input$sel_family)
      # rv$data_aggreg <- get_biomass(rv$data_filtered %>% 
      #                                 dplyr::filter(family %in% rv$sel_family),
      #                               'biomass_kg_ha')
      # rv$data_map <- get_biomass_loc(rv$data_aggreg)
    }, ignoreInit = TRUE
    )
  })
}
    
## To be copied in the UI
# mod_side_plot_ui("side_plot_ui_1")
    
## To be copied in the server
# mod_side_plot_server("side_plot_ui_1")

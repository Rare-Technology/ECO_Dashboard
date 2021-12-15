#' side_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput multiInput switchInput radioGroupButtons updatePickerInput
#' @importFrom leaflet providers
#' @importFrom shinyjs toggleState
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
      
      ui <- br()
      if (current_tab == "Visualize") {
        ui <- tagList(ui,
          div(class="sidetitle", "Plotting"),
          selectInput(ns('sel_metric'),
                      'Metric',
                      # todo: habitat diversity, habitat cover
                      # habitat diversity only worked for IDN in Abel's code
                      # habitat cover didn't work at all
                      choices = INIT$METRICS[["Fish"]]
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
      if (sel_dataset == "Fish") {
        ui <- tagList(ui,
          pickerInput(ns('sel_family'),
                      'Fish family',
                      choices = INIT$FAMILY$CHOICES,
                      selected = INIT$FAMILY$SELECTED,
                      options = list(
                        `actions-box` = TRUE,
                        `selected-text-format` = "count > 3"
                      ), multiple = TRUE)
        )
      }
      if (current_tab == 'Map') {
        ui <- tagList(ui,
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
    
    # observeEvent(rv$sel_year, {
    #   if (rv$current_tab %in% FISH_TABS) {
    #     family_choices <- get_geo_choices(INIT$DATA_FULL[[rv$sel_dataset]],
    #                                       sel_country = rv$sel_country,
    #                                       sel_subnational = rv$sel_subnational,
    #                                       sel_local = rv$sel_local,
    #                                       sel_maa = rv$sel_maa,
    #                                       sel_year = rv$sel_year,
    #                                       target = "family")
    #     updatePickerInput(
    #       session,
    #       'sel_family',
    #       choices = family_choices,
    #       selected = family_choices
    #     )
    #   }
    # })
    # 
    # observeEvent(rv$sel_maa, { # in case a change in maa does not yield a change in year
    #   if (rv$current_tab %in% FISH_TABS) {
    #     family_choices <- get_geo_choices(INIT$DATA_FULL[[rv$sel_dataset]],
    #                                       sel_country = rv$sel_country,
    #                                       sel_subnational = rv$sel_subnational,
    #                                       sel_local = rv$sel_local,
    #                                       sel_maa = rv$sel_maa,
    #                                       sel_year = rv$sel_year,
    #                                       target = "family")
    #     updatePickerInput(
    #       session,
    #       'sel_family',
    #       choices = family_choices,
    #       selected = family_choices
    #     )
    #   }
    # })
    
    
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
    
    observeEvent(rv$sel_year, {
      if (!is.null(rv$sel_maa)) {
        family_choices <- get_geo_choices(INIT$DATA_FULL[[rv$sel_dataset]],
                                          # sel_country = rv$sel_country,
                                          # sel_subnational = rv$sel_subnational,
                                          # sel_local = rv$sel_local,
                                          sel_maa = rv$sel_maa,
                                          sel_year = rv$sel_year,
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
    
    observeEvent(rv$sel_maa, {
      toggleState(id = "sel_family",
                  condition = !is.null(rv$sel_maa))
      if (!is.null(rv$sel_maa)) {
        family_choices <- get_geo_choices(INIT$DATA_FULL[[rv$sel_dataset]],
                                          sel_country = rv$sel_country,
                                          sel_subnational = rv$sel_subnational,
                                          sel_local = rv$sel_local,
                                          sel_maa = rv$sel_maa,
                                          sel_year = rv$sel_year,
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
      # rv$data_filtered <- INIT$DATA_FULL[[rv$sel_dataset]] %>% 
      #   dplyr::filter(country == rv$sel_country,
      #                 level1_name %in% rv$sel_subnational,
      #                 level2_name %in% rv$sel_local,
      #                 ma_name %in% rv$sel_maa,
      #                 year == rv$sel_year,
      #                 family %in% input$sel_family)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  })
}
    
## To be copied in the UI
# mod_side_plot_ui("side_plot_ui_1")
    
## To be copied in the server
# mod_side_plot_server("side_plot_ui_1")

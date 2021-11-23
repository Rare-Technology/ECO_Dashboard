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
      current_tab <- rv$current_tab
      
      if (current_tab %in% c("Start", "Fish", "Map")) {
        init_country_choices <- INIT$COUNTRY_CHOICES$FISH
        init_subnational_choices <- INIT$SUBNATIONAL_CHOICES$FISH
        init_local_choices <- INIT$LOCAL_CHOICES$FISH
        init_maa_choices <- INIT$MAA_CHOICES$FISH
      } else if (current_tab == "Mangrove Forests") {
        init_country_choices <- INIT$COUNTRY_CHOICES$MANGROVES
        init_subnational_choices <- INIT$SUBNATIONAL_CHOICES$MANGROVES
        init_local_choices <- INIT$LOCAL_CHOICES$MANGROVES
        init_maa_choices <- INIT$MAA_CHOICES$MANGROVES
      }
        
      ui <- tagList(
        div(class="sidetitle", "Geography"),
        selectInput(
          ns('sel_country'),
          'Country',
          choices = init_country_choices
        ),
        pickerInput(
          ns('sel_subnational'),
          'Subnational government',
          choices = init_subnational_choices,
          selected = init_subnational_choices,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = "count > 2"
          )
        ),
        pickerInput(
          ns('sel_local'),
          'Local government',
          choices = init_local_choices,
          selected = init_local_choices,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = "count > 2"
          )
        ),
        pickerInput(
          ns('sel_maa'),
          'Managed Access Area',
          choices = init_maa_choices,
          selected = NULL,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = "count > 2"
          )
        )
      )
     
      # if (current_tab %in% c("Coral Reefs", "Map")) {
      #   ui <- tagList(ui,
      #     pickerInput(
      #       ns('sel_maa'),
      #       'Managed access area',
      #       choices = init_maa_choices,
      #       selected = NULL,
      #       multiple = TRUE,
      #       options = list(
      #         `actions-box` = TRUE,
      #         `selected-text-format` = "count > 2"
      #       )
      #     )
      #   )
      # }
      ui
    })
    
    observeEvent(rv$sel_year, {
      sel_year <- rv$sel_year
      current_tab <- rv$current_tab
      
      if (current_tab %in% FISH_TABS) {
        data_year <- rv$data_full$fish %>% dplyr::filter(year == sel_year)
      } else if (current_tab == "Mangrove Forests") {
        data_year <- rv$data_full$mangroves %>% dplyr::filter(year == sel_year)
      }
      country_choices <- get_geo_choices(data_year, "country")
      updateSelectInput(
        session,
        "sel_country",
        choices = country_choices
      )
    }, ignoreInit = TRUE
    )
    
    observeEvent(input$sel_country, {
      rv$sel_country <- input$sel_country
      current_tab <- rv$current_tab
      sel_year <- rv$sel_year
      
      if (current_tab %in% FISH_TABS) {
        data_country <- rv$data_full$fish %>%
          dplyr::filter(year == sel_year,
                        country == input$sel_country)
        # rv$data_filtered$fish <- data_filtered
      } else if (current_tab == "Mangrove Forests") {
        data_country <- rv$data_full$mangroves %>% 
          dplyr::filter(year == sel_year,
                        country == input$sel_country)
        # rv$data_filtered$mangroves <- data_filtered
      }
      subnational_choices <- get_geo_choices(data_country, 'level1_name')
      updatePickerInput(
        session,
        'sel_subnational',
        choices = subnational_choices,
        selected = subnational_choices
      )
    }, ignoreInit = TRUE
    )
    
    observeEvent(input$sel_subnational, {
      rv$sel_subnational <- input$sel_subnational
      current_tab <- rv$current_tab
      sel_year <- rv$sel_year
      if (current_tab %in% FISH_TABS) {
        data_subnational <- rv$data_full$fish %>%
          dplyr::filter(year == sel_year,
                        level1_name %in% input$sel_subnational)
      } else if (current_tab == "Mangrove Forests") {
        data_subnational <- rv$data_full$mangroves %>%
          dplyr::filter(year == sel_year,
                        level1_name %in% input$sel_subnational)
      }
      local_choices <- get_geo_choices(data_subnational, 'level2_name')
      updatePickerInput(
        session,
        'sel_local',
        choices = local_choices,
        selected = local_choices
      )
    }, ignoreInit=TRUE
    )
    
    observeEvent(input$sel_local, {
      rv$sel_local <- input$sel_local
      current_tab <- rv$current_tab
      sel_year <- rv$sel_year
      if (current_tab %in% FISH_TABS) {
        data_local <- rv$data_full$fish %>%
          dplyr::filter(year == sel_year,
                        level2_name %in% input$sel_local)
      } else if (current_tab == "Mangrove Forests") {
        data_local <- rv$data_full$mangroves %>%
          dplyr::filter(year == sel_year,
                        level2_name %in% input$sel_local)
      }
      maa_choices <- get_geo_choices(data_local, 'ma_name')
      
      updatePickerInput(
        session,
        'sel_maa',
        choices = maa_choices,
        selected = NULL
      )
    },
    ignoreInit=TRUE
    )
    
    observeEvent(input$sel_maa, {
      rv$sel_maa <- input$sel_maa
      current_tab <- rv$current_tab
      sel_year <- rv$sel_year
      
      if (current_tab %in% FISH_TABS) {
        rv$data_filtered$fish <- rv$data_full$fish %>%
          dplyr::filter(year == sel_year,
                        ma_name %in% input$sel_maa)
      } else if (current_tab == "Mangrove Forests") {
        rv$data_filtered$mangroves <- rv$data_full$mangroves %>%
          dplyr::filter(year == sel_year,
                        ma_name %in% input$sel_maa)
      }
    }, ignoreInit = TRUE
    )
  })
}
    
## To be copied in the UI
# mod_side_geography_ui("side_geography_ui_1")
    
## To be copied in the server
# mod_side_geography_server("side_geography_ui_1")

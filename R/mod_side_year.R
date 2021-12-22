#' side_year UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs toggleState
sidebarYearUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("year"))
}
    
#' side_year Server Functions
#'
#' @noRd 
sidebarYearServer <- function(id, rv){
  ns <- NS(id)
  moduleServer( id, function(input, output, session){
    output$year <- renderUI({
      ui <- tagList(
        div(class = "sidetitle", tr(rv, "Survey Year")),
        selectInput(
          ns("sel_year"),
          tr(rv, "Year"),
          choices = INIT$YEAR$CHOICES,#year_choices,
          selected = INIT$YEAR$SELECTED#max(year_choices)
        )
      )
      
      ui
    })
    
    observeEvent(rv$sel_maa, {
        toggleState(id = "sel_year",
                    condition = !is.null(rv$sel_maa))
        if (!is.null(rv$sel_maa)) {
          year_choices <- get_geo_choices(INIT$DATA_FULL[[rv$sel_dataset]],
                                        sel_country = rv$sel_country,
                                        sel_subnational = rv$sel_subnational,
                                        sel_local = rv$sel_local,
                                        sel_maa = rv$sel_maa,
                                        target = "year")
          rv$sel_year <- max(year_choices)
          updateSelectInput(
            session,
            "sel_year",
            choices = year_choices,
            selected = max(year_choices)
          )
        }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    # observeEvent(rv$sel_maa, {
    #   year_choices <- get_geo_choices(INIT$DATA_FULL[[rv$sel_dataset]],
    #                                   sel_country = rv$sel_country,
    #                                   sel_subnational = rv$sel_subnational,
    #                                   sel_local = rv$sel_local,
    #                                   sel_maa = rv$sel_maa,
    #                                   target = "year")
    #   updateSelectInput(
    #     session,
    #     "sel_year",
    #     choices = year_choices,
    #     selected = max(year_choices)
    #   )
    #   
    #   # update plot data based on the ma selections changing
    #   # in case the selected year does not change in response to the selected ma's
    #   rv$data_filtered <- INIT$DATA_FULL[[rv$sel_dataset]] %>% 
    #     dplyr::filter(country == rv$sel_country,
    #                   level1_name %in% rv$sel_subnational,
    #                   level2_name %in% rv$sel_local,
    #                   ma_name %in% rv$sel_maa,
    #                   year == input$sel_year)
    # }, ignoreInit = TRUE)
    # 
    # observeEvent(input$sel_year, {
    #   rv$sel_year <- input$sel_year
    #   
    #   # if we change the year without changing ma, update the plot data
    #   rv$data_filtered <- INIT$DATA_FULL[[rv$sel_dataset]] %>% 
    #     dplyr::filter(country == rv$sel_country,
    #                   level1_name %in% rv$sel_subnational,
    #                   level2_name %in% rv$sel_local,
    #                   ma_name %in% rv$sel_maa,
    #                   year == input$sel_year)
    # }, ignoreInit = TRUE)
    
    observeEvent(input$sel_year, {
      if (rv$sel_year != input$sel_year) {
        rv$sel_year <- input$sel_year
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  })
}
    
## To be copied in the UI
# mod_side_year_ui("side_year_ui_1")
    
## To be copied in the server
# mod_side_year_server("side_year_ui_1")

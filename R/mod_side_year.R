#' side_year UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
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
      current_tab <- rv$current_tab
      
      if (current_tab %in% FISH_TABS) {
        init_year_choices <- INIT$YEAR_CHOICES$FISH
      } else if (current_tab == "Mangrove Forests") {
        init_year_choices <- INIT$YEAR_CHOICES$MANGROVES
      }
      
      ui <- tagList(
        div(class = "sidetitle", "Survey Year"),
        selectInput(
          ns("sel_year"),
          "Year",
          choices = init_year_choices,
          selected = max(init_year_choices)
        )
      )
      
      ui
    })
    
    observeEvent(input$sel_year, {
      rv$sel_year <- input$sel_year
    }, ignoreInit = TRUE
    )
  })
}
    
## To be copied in the UI
# mod_side_year_ui("side_year_ui_1")
    
## To be copied in the server
# mod_side_year_server("side_year_ui_1")

#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mainUI <- function(id){
  ns <- NS(id)
  fillPage(sidebarLayout(
    sidebarUI('sidebarUI'),
    mainPanel(
      tabsetPanel(
        id = ns('tabs'),
        tabPanel('Coral Reefs', plotUI('plotUI')),
        # tabPanel('Mangrove Forests', mangroveUI('mangroveUI')),
        tabPanel('Map', mapUI('mapUI'))
        # tabPanel('Report', reportUI('reportUI'))
      )
    ) #mainPanel
  )) # sidebarLayout, fixedPage
}
    
#' main Server Functions
#'
#' @noRd 
mainServer <- function(id, rv){
  moduleServer( id, function(input, output, session){
    observeEvent(input$tabs, {
      rv$current_tab <- input$tabs
    })
  })
}
    
## To be copied in the UI
# mod_main_ui("main_ui_1")
    
## To be copied in the server
# mod_main_server("main_ui_1")

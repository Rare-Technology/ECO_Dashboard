#' start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
startUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("message"))
}
    
#' start Server Functions
#'
#' @noRd 
startServer <- function(id, rv){
  ns <- NS(id)
  moduleServer( id, function(input, output, session){
    output$message <- renderUI({
      ui <- div(class='startpage',
                tags$h1(tr(rv,"Welcome!")),
                tags$p(tr(rv, "To get started, click on"), icon('filter'),
                  tr(rv, "to select a country then select the managed access areas you would like to see. For more information, click on"),
                  icon("question-circle-o")
                ),
                div(class = 'timeouttxt',
                    h3(class = 'timeouttitle', tr(rv, "Please note:")),
                    p(tr(rv, "This app may time-out if left idle too long, which will cause the screen to grey-out. To use the app again, refresh the page."))
                ),
                tags$img(id="start_banner", src="www/start_banner.png"),
      )
      ui
    })
  })
}
    
## To be copied in the UI
# mod_start_ui("start_ui_1")
    
## To be copied in the server
# mod_start_server("start_ui_1")

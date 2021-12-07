#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets dropdown
#' @importFrom shinyjs useShinyjs extendShinyjs
mainUI <- function(id){
  ns <- NS(id)
  fillPage(
    useShinyjs(),
    extendShinyjs(script="www/toggleFullScreen.js", functions=c("toggleFullScreen")),
    div(class='flow-div',
        dropdown(id='step1',
                 sidebarUI("sidebarUI"),
                 width='300px',
                 size='sm',
                 icon = icon('filter'),
                 status = 'success',
                 style='material-circle'
        ),
        div(style='flex-grow: 1;'),
        # div(id='help-button', icon('question-circle-o'), onclick='tour()'),
        div(class = 'fs-button',
            HTML(
              "<svg xmlns='http://www.w3.org/2000/svg' width='24' height='24' fill='currentColor' class='bi bi-fullscreen' viewBox='0 0 16 16'>
          <path d='M1.5 1a.5.5 0 0 0-.5.5v4a.5.5 0 0 1-1 0v-4A1.5 1.5 0 0 1 1.5 0h4a.5.5 0 0 1 0 1h-4zM10 .5a.5.5 0 0 1 .5-.5h4A1.5 1.5 0 0 1 16 1.5v4a.5.5 0 0 1-1 0v-4a.5.5 0 0 0-.5-.5h-4a.5.5 0 0 1-.5-.5zM.5 10a.5.5 0 0 1 .5.5v4a.5.5 0 0 0 .5.5h4a.5.5 0 0 1 0 1h-4A1.5 1.5 0 0 1 0 14.5v-4a.5.5 0 0 1 .5-.5zm15 0a.5.5 0 0 1 .5.5v4a1.5 1.5 0 0 1-1.5 1.5h-4a.5.5 0 0 1 0-1h4a.5.5 0 0 0 .5-.5v-4a.5.5 0 0 1 .5-.5z'/>
        </svg>"),
            onclick = "shinyjs.toggleFullScreen();"
        )
    ),
    tabsetPanel(
      id = ns('tabs'),
      tabPanel("Start", startUI("startUI")),
      tabPanel('Fish', plotUI('plotUI')),
      # tabPanel('Mangrove Forests', mangroveUI('mangroveUI')),
      # tabPanel('Map', mapUI('mapUI'))
      # tabPanel('Report', reportUI('reportUI'))
    )
  )
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

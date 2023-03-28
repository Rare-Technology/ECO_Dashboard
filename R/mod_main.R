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
    tagList(
      useShinyjs(),
      extendShinyjs(script="www/toggleFullScreen.js", functions=c("toggleFullScreen")),
      div(class='flow-div',
          uiOutput(ns("resetFiltersUI")),
          div(style='flex-grow: 1;'),
          # div(id='help-button', icon('question-circle-o'), onclick='tour()'),
          div(id="lang-select", selectInput(ns("language"), "", width = 80,
                                            c("EN" = "English",
                                              "ID" = "Bahasa Indonesia",
                                              "BR" = "Português (BRA)",
                                              "PT" = "Português (MOZ)",
                                              "ES" = "Español",
                                              "PH" = "Philippines"
                                            ))
          ),
          div(class = 'fs-button',
              HTML(
                "<svg xmlns='http://www.w3.org/2000/svg' width='24' height='24' fill='currentColor' class='bi bi-fullscreen' viewBox='0 0 16 16'>
            <path d='M1.5 1a.5.5 0 0 0-.5.5v4a.5.5 0 0 1-1 0v-4A1.5 1.5 0 0 1 1.5 0h4a.5.5 0 0 1 0 1h-4zM10 .5a.5.5 0 0 1 .5-.5h4A1.5 1.5 0 0 1 16 1.5v4a.5.5 0 0 1-1 0v-4a.5.5 0 0 0-.5-.5h-4a.5.5 0 0 1-.5-.5zM.5 10a.5.5 0 0 1 .5.5v4a.5.5 0 0 0 .5.5h4a.5.5 0 0 1 0 1h-4A1.5 1.5 0 0 1 0 14.5v-4a.5.5 0 0 1 .5-.5zm15 0a.5.5 0 0 1 .5.5v4a1.5 1.5 0 0 1-1.5 1.5h-4a.5.5 0 0 1 0-1h4a.5.5 0 0 0 .5-.5v-4a.5.5 0 0 1 .5-.5z'/>
          </svg>"),
              onclick = "shinyjs.toggleFullScreen();"
          )
      ),
      sidebarLayout(
        sidebarPanel(sidebarUI("sidebarUI"), width = 4) %>% tagAppendAttributes(id = 'filters-sidebar'),
        mainPanel(uiOutput(ns("tabPanels")), width = 8) %>% tagAppendAttributes(id = 'main-panel')
      ) %>% tagAppendAttributes(id = 'sidebar-layout'),
    ),
  padding = c(0, 15, 0, 15)
  )
}
    
#' main Server Functions
#'
#' @noRd 
mainServer <- function(id, rv){
  ns <- NS(id)
  moduleServer( id, function(input, output, session){
    output$resetFiltersUI <- renderUI({
      div(id="reset-filters", actionButton(ns('resetFilters'), tr(rv, "Reset filters")))
    })
    
    output$tabPanels <- renderUI({
      ui <- tabsetPanel(
        id = ns("tabs"),
        # tabPanel(tr(rv, "Start"), startUI("startUI")),
        tabPanel(tr(rv, "Visualize data"), plotUI('plotUI')),
        # tabPanel('Map', mapUI('mapUI')) # currently inactive, will rework soon
        tabPanel(tr(rv, "Report"), reportUI('reportUI'))
      )
      ui
    })
    
    observeEvent(input$tabs, {
      rv$current_tab <- input$tabs
    })
    
    observeEvent(input$language, {
      rv$language <- input$language
    })
    
    observeEvent(input$resetFilters, {
      state$resetFilters <- input$resetFilters + 1
    })
    
  })
}
    
## To be copied in the UI
# mod_main_ui("main_ui_1")
    
## To be copied in the server
# mod_main_server("main_ui_1")

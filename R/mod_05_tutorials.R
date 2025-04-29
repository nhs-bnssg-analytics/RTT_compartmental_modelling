#' 05_how_tos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_05_tutorials_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' 05_how_tos Server Functions
#'
#' @noRd
mod_05_tutorials_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_05_tutorials_ui("05_tutorials_1")

## To be copied in the server
# mod_05_tutorials_server("05_tutorials_1")

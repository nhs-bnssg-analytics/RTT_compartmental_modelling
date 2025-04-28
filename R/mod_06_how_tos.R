#' 06_how_tos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_06_how_tos_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 06_how_tos Server Functions
#'
#' @noRd 
mod_06_how_tos_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_06_how_tos_ui("06_how_tos_1")
    
## To be copied in the server
# mod_06_how_tos_server("06_how_tos_1")

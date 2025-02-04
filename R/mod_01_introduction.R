#' 01_introduction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_01_introduction_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 01_introduction Server Functions
#'
#' @noRd 
mod_01_introduction_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_01_introduction_ui("01_introduction_1")
    
## To be copied in the server
# mod_01_introduction_server("01_introduction_1")

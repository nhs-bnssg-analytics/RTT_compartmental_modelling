#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  r <- reactiveValues()


  mod_01_introduction_server("01_introduction_1")
  mod_02_planner_server("02_planner_1", r = r)
  mod_03_results_server("03_results_1", r = r)
  mod_04_downloads_server("04_downloads_1", r = r)
  mod_05_definitions_server("05_definitions_1")
  mod_06_how_tos_server("06_how_tos_1")
  mod_07_stories_server("07_stories_1")
}

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

  mod_08_batch_server("08_batch_1")

  mod_04_definitions_server("04_definitions_1")
  mod_05_tutorials_server("05_tutorials_1")
  mod_06_acknowledgements_server("06_acknowledgements_1")
}

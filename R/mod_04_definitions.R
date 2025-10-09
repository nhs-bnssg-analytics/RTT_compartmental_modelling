#' 04_definitions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_04_definitions_ui <- function(id) {
  ns <- NS(id)

  definitions <- definitions() |>
    (\(x) x[sort(names(x))])()

  page_fluid(
    title = "RTT Planner definitions",
    # theme = bs_theme(bootswatch = "flatly"),

    h1("RTT Planner terminology glossary"),
    p(HTML(
      "A quick reference guide to the terms used within this tool.
      See <a href='https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/02/Recording-and-reporting-RTT-waiting-times-guidance-v5.0-Feb25.pdf' target='_blank'>this document</a> for detailed definitions on RTT pathways."
    )),

    card(
      card_header("Definitions"),
      card_body(
        lapply(names(definitions), function(term) {
          div(
            HTML(
              paste0(
                "<strong>",
                term,
                "</strong> - ",
                definitions[[term]]
              )
            ),
            hr()
          )
        })
      ),
      full_screen = TRUE
    )
  )
}

#' 04_definitions Server Functions
#'
#' @noRd
mod_04_definitions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_04_definitions_ui("04_definitions_1")

## To be copied in the server
# mod_04_definitions_server("04_definitions_1")

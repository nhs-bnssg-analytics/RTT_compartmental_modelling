#' 05_definitions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_05_definitions_ui <- function(id){
  ns <- NS(id)

  definitions <- list(
    "Treatment capacity" = HTML(paste0("A 'clock stop' as a result of treatment. See <a href='https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/02/Recording-and-reporting-RTT-waiting-times-guidance-v5.0-Feb25.pdf'>this document</a> for more detailed definitions.")),
    "Renege" = "A 'clock stop' as a result of leaving the RTT pathway. This could be due to a number of reasons like taking private treatment, moving to another pathway, or dying etc.",
    "Referral" = HTML(paste0("A 'clock start', when a pathway begins. See <a href='https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/02/Recording-and-reporting-RTT-waiting-times-guidance-v5.0-Feb25.pdf'>this document</a> for more detailed definitions.")),
    "RTT" = "Referral to Treatment.",
    "Performance" = "The proportion of the RTT waiting list that have been waiting less than four months.",
    "Waiting list" = "The number of people that have been referred to treatment ('clock start'), but are yet to begin consultant-led treatment ('clock-stop')."
  ) |>
    (\(x) x[sort(names(x))])()

  page_fluid(
    title = "RTT Planner Definitions",
    # theme = bs_theme(bootswatch = "flatly"),

    h1("RTT Planner Terminology Glossary"),
    p("A quick reference guide to the terms used within this tool."),

    card(
      card_header("Definitions"),
      card_body(
        lapply(names(definitions), function(term) {
          div(
            h4(
              term,
              class = "definition"
            ),
            p(definitions[[term]]),
            hr()
          )
        })
      ),
      full_screen = TRUE
    )
  )
}

#' 05_definitions Server Functions
#'
#' @noRd
mod_05_definitions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_05_definitions_ui("05_definitions_1")

## To be copied in the server
# mod_05_definitions_server("05_definitions_1")

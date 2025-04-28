#' 05_definitions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_04_definitions_ui <- function(id){
  ns <- NS(id)

  definitions <- list(
    "Treatment capacity" = HTML(paste0("A 'clock stop' as a result of treatment. See <a href='https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/02/Recording-and-reporting-RTT-waiting-times-guidance-v5.0-Feb25.pdf'>this document</a> for more detailed definitions.")),
    "Renege" = "A 'clock stop' as a result of leaving the RTT pathway. This could be due to a number of reasons like taking private treatment, moving to another pathway, or dying etc.",
    "Referral" = HTML(paste0("A 'clock start', when a pathway begins. See <a href='https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/02/Recording-and-reporting-RTT-waiting-times-guidance-v5.0-Feb25.pdf'>this document</a> for more detailed definitions.")),
    "RTT" = "Referral to Treatment.",
    "Performance" = "The proportion of the RTT waiting list that have been waiting less than four months.",
    "Waiting list" = "The number of people that have been referred to treatment ('clock start'), but are yet to begin consultant-led treatment ('clock stop').",
    "Skew" = "Adjust the capacity utilisation profile (see above for definition) to focus more on longer waiters than shorter waiters (a skew value of greater than 1), or vice versa (a skew value of less than 1). In all scenarios, it is assumed the people waiting 0-1 months that are treated are 'urgent', and so the capacity utilisation for this group remains unchanged.",
    "Capacity utilisation profile" = "The model calibration process calculates the average rate that people have been treated by the number of months they have been waiting. This is calculated for those waiting up to 1 month, all the way up to those waiting 12+ months. These rates are the 'capacity utilisation profile'.",
    "18 week performance" = "The public data are published monthly, therefore permitting monthly modelling only. 18 weeks is, on average, 5 days less than 4 months. For ease of translating the tool into NHS target terms, the tool presents the 4 month performance as '18 weeks'."
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

#' 05_definitions Server Functions
#'
#' @noRd
mod_04_definitions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_05_definitions_ui("05_definitions_1")

## To be copied in the server
# mod_05_definitions_server("05_definitions_1")

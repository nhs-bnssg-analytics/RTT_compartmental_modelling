#' 04_definitions UI Function
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
    "Treatment capacity" = "A 'clock stop' as a result of leaving the RTT pathway due to treatment, or for other reasons as described by section 4 of the document linked to at the top of this page.",
    "Renege" = "A 'clock stop' as a result of leaving the RTT pathway for reasons other than treatment. This could be because of inter-provider transfers, or a clock stop that was not captured in the data submissions, for example.",
    "Referral" = "A 'clock start', when an RTT pathway begins.",
    "RTT" = "Referral to Treatment. Here an RTT pathway specifies a time from clock start to clock stop.",
    "Performance" = "The proportion of the RTT waiting list that have been waiting less than 18 weeks (four months).",
    "Waiting list" = "The number of people that have been referred to treatment ('clock start'), but are yet to begin consultant-led treatment ('clock stop').",
    "Skew" = "Adjust the capacity utilisation profile (see above for definition) to focus more on longer waiters than shorter waiters (a skew value of greater than 1), or vice versa (a skew value of less than 1). In all scenarios, it is assumed the people waiting 0-1 months that are treated are 'urgent', and so the capacity utilisation for this group remains unchanged.",
    "Capacity utilisation profile" = "The model calibration process calculates the average rate that people have been treated by the number of months they have been waiting. This is calculated for those waiting up to 1 month, all the way up to those waiting 12+ months. These rates are the 'capacity utilisation profile'.",
    "18 week performance" = "The public data are published monthly, therefore permitting monthly modelling only. 18 weeks is, on average, 5 days less than 4 months. For ease of translating the tool into NHS target terms, the tool presents the 4 month performance as '18 weeks'."
  ) |>
    (\(x) x[sort(names(x))])()

  page_fillable(
    title = "RTT Planner definitions",

    h1("RTT Planner terminology glossary"),
    p(HTML("A quick reference guide to the terms used within this tool.
      See <a href='https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/02/Recording-and-reporting-RTT-waiting-times-guidance-v5.0-Feb25.pdf'>this document</a> for detailed definitions on RTT pathways.")),

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
mod_04_definitions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_04_definitions_ui("04_definitions_1")

## To be copied in the server
# mod_04_definitions_server("04_definitions_1")

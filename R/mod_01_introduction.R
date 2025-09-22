#' 01_introduction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom bslib page_fillable page_navbar layout_column_wrap card
#'   card_header card_body card_footer
#' @importFrom shiny NS tagList h3 p hr actionButton div h4
mod_01_introduction_ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      col_widths = c(12),
      card(
        card_body(
          h3("About this tool"),
          p(
            "This is a tool to help the NHS plan and manage elective waiting lists and waiting times."
          ),

          p(
            "This tool projects future waiting list and associated performance metrics based on a model calibrated
            on historical referral, treatment and reneging activity. By applying the calibrated model to referral
            trajectories, the tool can help users make informed decisions to improve patient care
            delivery and achieve NHS performance targets."
          ),

          p(HTML(
            "This tool has been developed, and is maintained, by the South West Decision Support Network.
            See the <em>Acknowledgements</em> tab for its history and to find out more about the collaborators."
          )),

          p(
            HTML(
              paste0(
                "See the latest features <a href='https://github.com/nhs-bnssg-analytics/RTT_compartmental_modelling/blob/main/NEWS.md'>here</a>."
              )
            )
          ),

          hr(),

          h3("How to use this tool"),
          tableOutput(ns("overview_table")),
          hr(),

          h3("Future developments"),
          p(
            "We are continuously improving this tool to better serve healthcare professionals.
          The following enhancements are currently in development:"
          ),

          tags$ul(
            tags$li(
              tags$strong("Translate treatment capacity to activity:"),
              " Apply national analaysis of pathways data to provide an estimate of the numbers and types of activity per treatment."
            ),
            tags$li(
              tags$strong("Other:"),
              HTML(
                " View the issues backlog <a href='https://github.com/nhs-bnssg-analytics/RTT_compartmental_modelling/issues'>here</a>."
              )
            )
          ),

          hr(),
        )
      )
    )
  )
}

#' 01_introduction Server Functions
#' @importFrom shiny updateTabsetPanel renderTable
#' @noRd
mod_01_introduction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$overview_table <- renderTable(
      {
        data.frame(
          Objective = c(
            "Understand how treatment capacity impact performance",
            "Optimise short term treatment capacity to meet a performance target",
            "Understand how to get to a healthy waiting list size and shape"
          ),
          Tab = c("Scenario planner", "Scenario planner", "Steady state"),
          Description = c(
            paste(
              "Select trust(s) and specialty of interest, then download/upload data. ",
              "Enter the forecast period and referrals projection information.",
              "Select the scenario type 'Calculate performance (from treatment capacity inputs)'.",
              "Provide the treatment capacity information (along with skew settings).",
              "Hit 'Calculate future performance'.",
              "View the results in the 'Results' tab.",
              sep = "<br>"
            ),
            paste(
              "Select trust(s) and specialty of interest, then download/upload data. ",
              "Enter the forecast period and referrals projection information.",
              "Select the scenario type 'Calculate treatment capacity (from performance inputs)'.",
              "Provide the performance information (along with skew settings).",
              "Hit 'Optimise treatment capacity'.",
              "View the results in the 'Results' tab.",
              sep = "<br>"
            ),
            paste(
              "Select trusts and specialties of interest.",
              "Enter referrals scenarios.",
              "Set the target data and performance target.",
              "Choose solution method and hit 'Batch Run'.",
              sep = "<br>"
            )
          )
        )
      },
      bordered = TRUE,
      sanitize.text.function = identity
    )
  })
}

## To be copied in the UI
# mod_01_introduction_ui("01_introduction_1")

## To be copied in the server
# mod_01_introduction_server("01_introduction_1")

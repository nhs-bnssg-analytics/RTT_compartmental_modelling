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
mod_01_introduction_ui <- function(id){
  ns <- NS(id)
  page_fillable(
    # theme = bs_theme(bootswatch = thm),
    layout_columns(
      col_widths = c(12),
      card(
        card_body(
          p("This is a tool to help the NHS plan to manage waiting times."),
          h3("About This Tool"),
          p("This planning tool helps healthcare planners to plan resources to meet hospital waiting list targets."),

          p("By calibrating on historical waiting list, referral and clock stop data, this tool provides forecasts
          of waiting lists and associated performance based on future projections of referrals, to help the user
          make informed decisions to improve patient care delivery and achieve NHS performance targets."),

          hr(),

          h3("How to Use This Tool"),
          p("Navigate through the following sections using the tabs at the top of each page:"),

          layout_column_wrap(
            col_widths = c(6, 6),
            fill = FALSE,
            card(
              card_header(
                h4("1. Calibrate the model (Scenario planner)"),
                class = "intro-card"
              ),
              card_body(
                p("Make selections of trust, specialty, and any associated commissioning groups."),
                p("Select the length of time that the calibration period should be."),
                p("Hit the Download button.")
              )
            ),

            card(
              card_header(
                h4("2a. Calculate performance (Scenario planner)"),
                class = "intro-card"
              ),
              card_body(
                p("Enter the forecast period and referrals projection information."),
                p("Select the scenario type 'Enter performance (from capacity inputs)'"),
                p("Provide the capacity information (along with skew settings)."),
                p("Hit 'Calculate future performance'.")
              )
            ),

            card(
              card_header(
                h4("2b. Optimise capacity (Scenario planner)"),
                class = "intro-card"
              ),
              card_body(
                p("Enter the forecast period and referrals projection information."),
                p("Select the scenario type 'Enter capacity (from performance inputs)'"),
                p("Provide the performance information (along with skew settings)."),
                p("Hit 'Run capacity optimisation'.")
              )
            ),

            card(
              card_header(
                h4("3. View results (Results and Downloads)"),
                class = "intro-card"
              ),
              card_body(
                p("View charts on Results tab"),
                p("Download report on Downloads tab."),
                p("Adjust scenario by clicking 'Use results as planning inputs'."),
                p("Re-run steps 2a or 2b.")
              )
            )
          ),

          hr(),

          h3("Future Developments"),
          p("We are continuously improving this tool to better serve healthcare professionals.
          The following enhancements are currently in development:"),

          tags$ul(
            tags$li(tags$strong("Translate 'clock-stops' to activity:"), " Apply national analaysis of pathways data to provide an estimate of the numbers and types of activity per clock-stop."),
            tags$li(tags$strong("Reporting:"), " Download a Word document with the information created within the tool."),
            tags$li(tags$strong("Post-optimisation adjustments:"), " Feed the projection data back into the scenario section to make minor, more custom adjustments."),
            tags$li(tags$strong("Data inputs:"), " Once data are downloaded, present visualisations of the inputs."),
            tags$li(tags$strong("Other:"), HTML(" View the issues backlog <a href='https://github.com/nhs-bnssg-analytics/RTT_compartmental_modelling/issues'>here</a>."))
          ),

          hr(),

        )
      )
    )
  )

}

#' 01_introduction Server Functions
#' @importFrom shiny updateTabsetPanel
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

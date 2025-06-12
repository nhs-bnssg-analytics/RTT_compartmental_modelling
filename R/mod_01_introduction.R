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
    layout_columns(
      col_widths = c(12),
      card(
        card_body(
          h3("About this tool"),
          p("This is a tool to help the NHS plan and manage elective waiting lists and waiting times."),

          p("This tool projects future waiting list and associated performance metrics based on a model calibrated
            on historical referral, treatment and reneging activity. By applying the calibrated model to referral
            trajectories, the tool can help users make informed decisions to improve patient care
            delivery and achieve NHS performance targets."),

          p(HTML("This tool has been developed, and is maintained, by the South West Decision Support Network.
            See the <em>Acknowledgements</em> tab for its history and to find out more about the collaborators.")),

          p(
            HTML(
              paste0(
                "See the latest features <a href='https://github.com/nhs-bnssg-analytics/RTT_compartmental_modelling/blob/main/NEWS.md'>here</a>."
                )
            )
          ),

          hr(),

          h3("How to use this tool"),
          p("Navigate through the following sections using the tabs at the top of each page:"),

          layout_column_wrap(
            # gap = "100px",
            width = 1 / 3,
            min_height = "800px",
            # First column with title above card
            div(
              h5("Step 1"),
              card(
                card_header(
                  h4(
                    HTML("Calibrate the model (<em>Scenario planner</em>)")
                  ),
                  class = "intro-card"
                ),
                card_body(
                  p("Make selections of trust, specialty, and any associated commissioning groups."),
                  p("Select the length of time that the calibration period should be."),
                  p(HTML("Hit the <em>Download</em> button."))
                )
              )
            ),

            # Second column with title above two stacked cards
            div(
              h5("Step 2"),
              layout_column_wrap(
                width = 1,
                heights_equal = "row",
                # style = css(grid_template_rows = "1fr 1fr"),
                #   fill = FALSE,
                #   fillable = FALSE,
                #   p("Step 2"),
                card(
                  card_header(
                    h4(
                      HTML("Calculate performance (<em>Scenario planner</em>)")
                    ),
                    class = "intro-card"
                  ),
                  card_body(
                    p("Enter the forecast period and referrals projection information."),
                    p(HTML("Select the scenario type <em>'Calculate performance (from treatment capacity inputs)'</em>.")),
                    p("Provide the treatment capacity information (along with skew settings)."),
                    p(HTML("Hit <em>'Calculate future performance'</em>."))
                  )
                ),

                h5("...or..."),

                card(
                  card_header(
                    h4(
                      HTML("Optimise treatment capacity (<em>Scenario planner</em>)")
                    ),
                    class = "intro-card"
                  ),
                  card_body(
                    p("Enter the forecast period and referrals projection information."),
                    p(HTML("Select the scenario type <em>'Calculate treatment capacity (from performance inputs)'</em>.")),
                    p("Provide the performance information (along with skew settings)."),
                    p(HTML("Hit <em>'Optimise treatment capacity'</em>."))
                  )
                )
              )
            ),


            # Third column with title above card
            div(
              h5("Step 3"),
              card(
                card_header(
                  h4(
                    HTML("View results (<em>Results</em>)")
                  ),
                  class = "intro-card"
                ),
                card_body(
                  p("View charts."),
                  p("Download data."),
                  p("Download report.")
                )
              )
            )
          ),

          hr(),

          h3("Future developments"),
          p("We are continuously improving this tool to better serve healthcare professionals.
          The following enhancements are currently in development:"),

          tags$ul(
            tags$li(tags$strong("Translate treatment capacity to activity:"), " Apply national analaysis of pathways data to provide an estimate of the numbers and types of activity per treatment."),
            tags$li(tags$strong("Post-optimisation adjustments:"), " Feed the projection data back into the scenario section to make minor, more custom adjustments."),
            tags$li(tags$strong("Data inputs:"), " Provide visualisations of the downloaded data once the data are downloaded."),
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

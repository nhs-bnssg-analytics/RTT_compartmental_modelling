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

  # create statement for data update information
  data_info <- system.file(
    "extdata",
    "run_date.rds",
    package = "RTTshiny"
  )
  if (data_info != "") {
    data_info <- readRDS(data_info) |>
      (\(x) {
        paste0(
          "Last data import: ",
          x["Data updated (12 month)"],
          "; last check for new data: ",
          x["Data checked"]
        )
      })()
  }

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
            HTML(paste(
              "This tool projects future",
              tooltip_label("waiting list"),
              "and associated",
              tooltip_label("performance"),
              "metrics based on a model calibrated
            on historical",
              tooltip_label("referral,", "referral"),
              ",",
              tooltip_label("treatment", "treatment capacity"),
              "and",
              tooltip_label("reneging", "renege"),
              "activity. By applying the calibrated model to",
              tooltip_label("referral"),
              "trajectories, the tool can help users make informed decisions to improve patient care
            delivery and achieve NHS",
              tooltip_label("performance"),
              "targets."
            ))
          ),

          p(HTML(
            "This tool has been developed, and is maintained, by the South West Decision Support Network.
            See the <em>Acknowledgements</em> tab for its history and to find out more about the collaborators."
          )),

          p(
            HTML(
              paste0(
                "See the latest features <a href='https://github.com/nhs-bnssg-analytics/RTT_compartmental_modelling/blob/main/NEWS.md' target='_blank'>here</a>."
              )
            )
          ),
          p(HTML(
            paste0(
              '<span style="font-size: 0.6rem;">',
              data_info,
              '</span>'
            )
          )),

          hr(),

          h3("How to use this tool"),
          tableOutput(ns("overview_table")),
          hr(),

          h3("Future developments"),
          p(
            "We are continuously improving this tool to better serve healthcare planning teams.
          The following enhancements are currently in development:"
          ),

          tags$ul(
            tags$li(
              tags$strong(HTML(paste(
                "Translate",
                tooltip_label("treatment capacity"),
                "to activity:"
              ))),
              " Apply national analaysis of pathways data to provide an estimate of the numbers and types of activity per treatment."
            ),
            tags$li(
              tags$strong("Other:"),
              HTML(
                " View the issues backlog <a href='https://github.com/nhs-bnssg-analytics/RTT_compartmental_modelling/issues' target='_blank'>here</a>."
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
            paste(
              "Understand how",
              tooltip_label("treatment capacity"),
              "impacts",
              tooltip_label("performance")
            ),
            paste(
              "Optimise short term",
              tooltip_label("treatment capacity"),
              "to meet a",
              tooltip_label("performance"),
              "target"
            ),
            paste(
              "Understand how to get to a",
              tooltip_label("healthy waiting list"),
              "size and shape"
            )
          ),
          Tab = c("Scenario planner", "Scenario planner", "Steady state"),
          Description = c(
            paste(
              "Select trust(s) and specialty of interest, then download/upload data. ",
              paste(
                "Enter the forecast period and",
                tooltip_label("referrals", "referral"),
                "projection information."
              ),
              "Select the scenario type 'Calculate performance (from treatment capacity inputs)'.",
              paste(
                "Provide the",
                tooltip_label("treatment capacity"),
                "information (along with",
                tooltip_label("skew"),
                "settings)."
              ),
              "Hit 'Calculate future performance'.",
              "View the results in the 'Results' tab.",
              sep = "<br>"
            ),
            paste(
              "Select trust(s) and specialty of interest, then download/upload data. ",
              paste(
                "Enter the forecast period and",
                tooltip_label("referrals", "referral"),
                "projection information."
              ),
              "Select the scenario type 'Calculate treatment capacity (from performance inputs)'.",
              paste(
                "Provide the",
                tooltip_label("performance"),
                "information (along with",
                tooltip_label("skew"),
                "settings)."
              ),
              "Hit 'Optimise treatment capacity'.",
              "View the results in the 'Results' tab.",
              sep = "<br>"
            ),
            paste(
              "Select trusts and specialties of interest.",
              paste(
                "Enter",
                tooltip_label("referrals", "referral"),
                "scenarios."
              ),
              paste(
                "Set the target data and",
                tooltip_label("performance"),
                "target."
              ),
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

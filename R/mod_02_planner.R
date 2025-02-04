#' 02_planner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib input_task_button card card_header layout_sidebar sidebar
#'   bs_theme page_fluid
mod_02_planner_ui <- function(id){
  ns <- NS(id)

  # date card
  date_card <- card(
    card_header("Select dates for analysis and forecasting"),
    dateRangeInput(
      inputId = ns("cal_date"),
      label = "Calibration date range:",
      min = "2016-03-01",
      start = "2023-12-01",
      end = "2024-11-30"
    ),
    dateRangeInput(
      inputId = ns("val_date"),
      label = "Validation date range:",
      min = "2016-03-01",
      start = "2022-12-01",
      end = "2023-11-30"

    ),
    dateRangeInput(
      inputId = ns("timeseries_date"),
      label = "Time series date range:",
      min = "2016-03-01",
      start = "2022-12-01",
      end = "2024-11-30"
    ),
    dateRangeInput(
      inputId = ns("forecast_date"),
      label = "Projection date range:",
      min = "2016-03-01",
      start = "2024-12-01",
      end = "2026-03-31"
    ),
  )

  filters_card <- card(
    card_header("Select filters on data"),
    selectInput(
      inputId = ns("trust_parent_codes"),
      label = "Select trust parent codes",
      choices = c("QE1", "QUY"),
      multiple = TRUE
    ),
    selectInput(
      inputId = ns("commissioner_parent_codes"),
      label = "Select commissioner parent codes",
      choices = c("QE1", "QUY"),
      multiple = TRUE
    ),
    selectInput(
      inputId = ns("commissioner_org_codes"),
      label = "Select commissioner org codes",
      choices = c("00R", "15M"),
      multiple = TRUE
    ),
    selectInput(
      inputId = ns("trust_codes"),
      label = "Select trust codes",
      choices = c("RBD", "RVJ"),
      multiple = TRUE
    ),
    selectInput(
      inputId = ns("specialty_codes"),
      label = "Select specialty codes",
      choices = c("C_999", "C_150"),
      multiple = TRUE
    ),
  )

  # configuration card
  selector_card <- card(
    card_header(
      "Make selections"
    ),
    date_card,
    filters_card,
    bslib::input_task_button(
      id = ns("dwnld_rtt_data"),
      label = "Download RTT data",
      label_busy = "Downloading...",
      type = "secondary"
    )
  )


  tagList(
    bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      bslib::layout_sidebar(
        sidebar = sidebar(
          selector_card,
          open = TRUE,
          width = '25%'
        )
      )
    )

  )
}

#' 02_planner Server Functions
#'
#' @importFrom shiny observeEvent
#' @importFrom NHSRtt get_rtt_data
#' @noRd
mod_02_planner_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(
      c(input$cal_date,
        input$val_date,
        input$timeseries_date,
        input$forecast_date), {

          # final calibration period
          calibration_start <- as.Date(input$cal_date[[1]])
          calibration_end <- as.Date(input$cal_date[[2]])

          # validate on the final calibration period
          validation_start <- as.Date(input$val_date[[1]])
          validation_end <- as.Date(input$val_date[[2]])

          # start date for tbats
          tbats_start <- as.Date(input$timeseries_date[[1]])
          tbats_end <- as.Date(input$timeseries_date[[2]])

          prediction_start <- as.Date(input$forecast_date[[1]])
          prediction_end <- as.Date(input$forecast_date[[2]])

          r$lower_date <- min(
            calibration_start,
            validation_start,
            tbats_start,
            na.rm =TRUE
          )

          r$upper_date <- max(
            calibration_end,
            validation_end,
            tbats_end,
            na.rm =TRUE
          )
        }
    )

    observeEvent(
      input$dwnld_rtt_data, {
        r$all_data <- NHSRtt::get_rtt_data(
          date_start = r$lower_date,
          date_end = r$upper_date,
          trust_parent_codes = input$trust_parent_codes,
          trust_codes = input$trust_codes,
          commissioner_parent_codes = input$commissioner_parent_codes,
          commissioner_org_codes = input$commissioner_org_codes,
          specialty_codes = input$specialty_codes
        )
      }
    )

  })
}

## To be copied in the UI
# mod_02_planner_ui("02_planner_1")

## To be copied in the server
# mod_02_planner_server("02_planner_1")


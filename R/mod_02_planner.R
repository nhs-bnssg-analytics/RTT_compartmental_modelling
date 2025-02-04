#' 02_planner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib navset_card_tab input_task_button card card_header
#'   card_body layout_column_wrap layout_sidebar sidebar bs_theme page_fluid
#'   nav_panel tooltip
mod_02_planner_ui <- function(id){
  ns <- NS(id)

  # date card
  date_card <- card(
    card_header("Select dates for analysis and forecasting"),
    dateRangeInput(inputId = "cal_date", label = "Calibration date range:"),
    dateRangeInput(inputId = "val_date", label = "Validation date range:"),
    dateRangeInput(inputId = "timeseries_date", label = "Time series date range:"),
    dateRangeInput(inputId = "forecast_date", label = "Projection date range:"),
  )

  filters_card <- card(
    card_header("Select filters on data"),
    selectInput(
      inputId = "trust_parent_codes",
      label = "Select trust parent codes",
      choices = c("QE1", "QUY"),
      multiple = TRUE
    ),
    selectInput(
      inputId = "commissioner_parent_codes",
      label = "Select commissioner parent codes",
      choices = c("C_999", "C_123"),
      multiple = TRUE
    ),
    selectInput(
      inputId = "commissioner_org_codes",
      label = "Select commissioner org codes",
      choices = c("C015", "C_123"),
      multiple = TRUE
    ),
    selectInput(
      inputId = "trust_codes",
      label = "Select specialty codes",
      choices = c("C_999", "C_123"),
      multiple = TRUE
    ),
    selectInput(
      inputId = "specialty_codes",
      label = "Select specialty codes",
      choices = c("C_999", "C_123"),
      multiple = TRUE
    ),


  )

  tagList(

  )
}

#' 02_planner Server Functions
#'
#' @noRd
mod_02_planner_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


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

    lower_date <- min(calibration_start, validation_start, tbats_start, prediction_start, na.rm =TRUE)
    upper_date <- max(calibration_end, validation_end, tbats_end, prediction_end, na.rm =TRUE)
  })
}

## To be copied in the UI
# mod_02_planner_ui("02_planner_1")

## To be copied in the server
# mod_02_planner_server("02_planner_1")


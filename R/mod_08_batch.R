#' 08_batch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput radioButtons numericInput
#' @importFrom bslib input_task_button card card_header layout_sidebar sidebar

# UI
mod_08_batch_ui <- function(id){
  ns <- NS(id)

  filters_sidebar <- sidebar(
    open = TRUE,
    width = '25%',
    selectizeInput(
      inputId = ns("trust_codes"),
      label = "Select Trust(s)",
      choices = sort(unique(org_lkp$`Provider Org Name`)),
      options = list(
        placeholder = "Select one or more"
      ),
      multiple = TRUE
    ),
    selectizeInput(
      inputId = ns("specialty_codes"),
      label = "Select specialties",
      selected = "Total",
      choices = unname(treatment_function_codes)[unname(treatment_function_codes) != "Total"], # Exclude Total
      options = list(
        placeholder = "Select one or more"
      ),
      multiple = TRUE # Enabled
    ),
    radioButtons(
      inputId = ns("referral_bin"),
      label = "Select a referral scenario:",
      choices = c(
        "Low (-1)" = -1,
        "Medium (0)" = 0,
        "High (1)" = 1
      ),
      selected = 0 # Default
    ),
    dateInput(
      inputId = ns("target_date"),
      label = "Target date",
      min = "2028-03-01",
      max = "2028-03-01",
      value = "2028-03-01",
      format = "dd-mm-yyyy",
      weekstart = 1,
      autoclose = TRUE,
      width = "40%"
    ),
    sliderInput(
      inputId = ns("target_bin"),
      label = "Target proportion",
      min = 0,
      max = 200,
      value = 92,
      step = 1,
      post = "%",
      width = "100%"
    ),
    hr(),
    layout_columns(
      col_widths = c(11, 1),
      bslib::input_task_button(
        id = ns("batch_run_rtt_data"),
        label = "Batch Run",
        label_busy = "Running...",
        type = "dark"
      ),
      uiOutput(ns("tick_mark_dwnld"))
    )
  )


  # Right Pane
  scenario_card <- card(
    card_header("Batch Output View"),
    card_body(
    hr(),
    helpText("Coming Soon...")
    ),
    fill = FALSE,
    min_height = 650
  )

  bslib::page_fillable(
    theme = bslib::bs_theme(version = 5),
    card(
      bslib::layout_sidebar(
        sidebar = filters_sidebar,
        scenario_card,
        fill = TRUE,
        fillable = TRUE
      )
    )
  )
} # End UI


# Server
mod_08_batch_server <- function(id, r){
  moduleServer(id, function(input, output, session){
  })
}



## To be copied in the UI
# mod_03_results_ui("03_results_1")

## To be copied in the server
# mod_03_results_server("03_results_1")

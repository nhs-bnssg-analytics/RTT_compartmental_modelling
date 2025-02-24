#' 02_planner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput radioButtons numericInput
#'   dateRangeInput dateInput selectInput
#' @importFrom bslib input_task_button card card_header layout_sidebar sidebar
#'   bs_theme page_fluid
mod_02_planner_ui <- function(id){
  ns <- NS(id)

  # date card
  scenario_card <- card(
    card_header("Select dates for analysis and forecasting"),

    # dateRangeInput(
    #   inputId = ns("cal_date"),
    #   label = "Calibration date range:",
    #   min = "2016-03-01",
    #   start = "2023-12-01",
    #   end = "2024-11-30"
    # ),
    uiOutput(
      ns("forecast_horizon")
    ),
    numericInput(
      inputId = ns("referral_growth"),
      label = "Percentage change in referrals (between -20% and 200%)",
      value = 0,
      min = -20,
      max = 200
    ),
    radioButtons(
      inputId = ns("referral_growth_type"),
      label = "Select type of referral change:",
      choices = c("Uniform", "Linear"),
      selected = "Linear"#,
      # choiceNames = c("Uplift referrals uniformly", "Uplift referrals to change by a percentage (linearly) by the end of the time period"),
      # choiceValues = c("uniform", "linear")
    ),
    selectInput(
      inputId = ns("interface_choice"),
      label = "Select scenario type",
      choices = c(
        "Estimate performance" = "performance",
        "Estimate capacity" = "capacity")
    ),
    uiOutput(
      ns("dynamic_interface")
    )



    # max_months_waited <- 12 # INPUT
    # target_waiting_period <- 4 # INPUT
    # speciatly_filter <- "C_100" # INPUT
    #
    #
    # referral_growth_type <- "uniform" # uniform or linear - INPUT
    # referral_growth <- 0.01 # INPUT
    # capacity_growth_type <- "uniform" # uniform or linear - INPUT
    # capacity_growth <- 0.01 # INPUT


  )

  filters_card <- card(
    card_header("Select filters on data"),
    # selectInput(
    #   inputId = ns("trust_parent_codes"),
    #   label = "Select trust parent codes",
    #   choices = c("QE1", "QUY"),
    #   multiple = TRUE
    # ),
    # selectInput(
    #   inputId = ns("commissioner_parent_codes"),
    #   label = "Select commissioner parent codes",
    #   choices = c("QE1", "QUY"),
    #   multiple = TRUE
    # ),
    # selectInput(
    #   inputId = ns("commissioner_org_codes"),
    #   label = "Select commissioner org codes",
    #   choices = c("00R", "15M"),
    #   multiple = TRUE
    # ),
    selectInput(
      inputId = ns("trust_codes"),
      label = "Select trust codes",
      choices = unname(trust_lkp),
      multiple = FALSE
    ),
    selectInput(
      inputId = ns("specialty_codes"),
      label = "Select specialty codes",
      choices = unname(treatment_function_codes),
      multiple = FALSE
    ),
    bslib::input_task_button(
      id = ns("dwnld_rtt_data"),
      label = "Download RTT data",
      label_busy = "Downloading...",
      type = "secondary"
    )
  )

  # configuration card
  selector_card <- card(
    card_header(
      "Make selections"
    ),
    filters_card,
    scenario_card

  )


  tagList(
    bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      # bslib::layout_sidebar(
      #   sidebar = sidebar(
          selector_card
      #     open = TRUE,
      #     width = '25%'
      #   )
      # )
    )

  )
}

#' 02_planner Server Functions
#'
#' @importFrom shiny observeEvent renderUI dateInput tagList
#'   numericInput
#' @importFrom shinyWidgets numericRangeInput
#' @importFrom NHSRtt get_rtt_data latest_rtt_date
#' @importFrom lubridate `%m+%` `%m-%` floor_date ceiling_date
#' @noRd
mod_02_planner_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(
      c(input$cal_date,
        input$forecast_date), {

          # final calibration period
          # r$lower_date <- as.Date(input$cal_date[[1]])
          # r$upper_date <- as.Date(input$cal_date[[2]])
          r$upper_date <- NHSRtt::latest_rtt_date()
          r$lower_date <- lubridate::floor_date(
            r$upper_date,
            unit = "months"
          ) %m-% months(11)

          prediction_start <- as.Date(input$forecast_date[[1]])
          prediction_end <- as.Date(input$forecast_date[[2]])

        }
    )


    observeEvent(
      input$dwnld_rtt_data, {
        r$all_data <- NHSRtt::get_rtt_data(
          date_start = r$lower_date,
          date_end = r$upper_date,
          # trust_parent_codes = input$trust_parent_codes,
          trust_codes = input$trust_codes,
          # commissioner_parent_codes = input$commissioner_parent_codes,
          # commissioner_org_codes = input$commissioner_org_codes,
          specialty_codes = input$specialty_codes
        )
      }
    )


# dynamic UI --------------------------------------------------------------

    # here, we force the target achievement date to fit into the forecast time period
    target_dates <- reactive({
      min_date <- as.Date(input$forecast_date[[1]]) %m+% months(1)
      max_date <- as.Date(input$forecast_date[[2]])

      target_dates <- list(
        min = min_date,
        max = max_date
      )
    })

    output$target_achievement_date <- shiny::renderUI(
      dateInput(
        inputId = ns("target_achievement_date"),
        label = "Select date to achieve target by",
        min = target_dates()$min,
        max =  target_dates()$max,
        value = target_dates()$max
      )
    )

    # create forecast horizon default dates
    forecast_dates <- reactive({
      start_date <- NHSRtt::latest_rtt_date() + 1
      end_date <- lubridate::ceiling_date(
        start_date,
        unit = "months"
      ) %m+% months(35)

      forecast_dates <- list(
        start = start_date,
        end = end_date
      )
    })


    output$forecast_horizon <- shiny::renderUI(
      dateRangeInput(
        inputId = ns("forecast_date"),
        label = "Forecast horizon date range:",
        min = "2016-05-01",
        start = forecast_dates()$start,
        end = forecast_dates()$end
      )
    )



    # Generate the dynamic UI based on dropdown selection
    output$dynamic_interface <- renderUI({
      if (input$interface_choice == "performance") {
        # Numeric interface
        tagList(
          uiOutput(
            ns("target_achievement_date")
          ),
          # dateInput(
          #   inputId = ns("target_achievement_date"),
          #   label = "Achieve target by"
          # ),
          numericInput(
            # INPUT (note, the package requires the 100% - x of this value, eg, 65% performance = a target_value of 35%)
            inputId = ns("target_value"),
            label = "Target percentage (between 0% and 100%)",
            min = 0,
            max = 100,
            value = 70
          ),
          numericRangeInput(
            inputId = ns("capacity_skew_range"),
            label = "Select range of capacity skews",
            value = c(0.8, 1.2),
            min = 0.1,
            max = 3, #this is arbitrary
            step = 0.05
          ),
          actionButton(
            inputId = ns("optimise_capacity"),
            label = "Run capacity optimisation",

          )
        )

      } else if (input$interface_choice == "capacity") {
        # Text interface
        tagList(
          numericInput(
            inputId = ns("capacity_growth"),
            label = "Percentage change for capacity (between -20% and 20%)",
            value = 0,
            min = -20,
            max = 200
          ),
          radioButtons(
            inputId = ns("capacity_growth_type"),
            label = "Select type of capacity change:",
            choices = c("Uniform", "Linear"),
            selected = "Linear"#,
            # choiceNames = c("Uplift referrals uniformly", "Uplift referrals to change by a percentage (linearly) by the end of the time period"),
            # choiceValues = c("uniform", "linear")
          ),
          numericInput(
            inputId = ns("capacity_skew"),
            label = "Enter capacity utilisation skew",
            value = 1,
            min = 0.1,
            max = 3, #this is arbitrary
            step = 0.05
          ),
          actionButton(
            inputId = ns("calculate_performance"),
            label = "Calculate future performance",

          )
        )
      }
    })


# Forecast performance based on capacity inputs ---------------------------



  })
}

## To be copied in the UI
# mod_02_planner_ui("02_planner_1")

## To be copied in the server
# mod_02_planner_server("02_planner_1")


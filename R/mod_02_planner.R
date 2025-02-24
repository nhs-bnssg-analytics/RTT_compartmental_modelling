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
#' @importFrom shiny observeEvent renderUI dateInput tagList numericInput
#'   eventReactive
#' @importFrom shinyWidgets numericRangeInput
#' @importFrom NHSRtt get_rtt_data latest_rtt_date convert_months_waited_to_id
#'   apply_params_to_projections
#' @importFrom lubridate `%m+%` `%m-%` floor_date ceiling_date interval
#' @importFrom stringr str_replace_all
#' @importFrom dplyr mutate summarise arrange row_number
#' @importFrom tidyr complete
#' @noRd
mod_02_planner_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # create period_lkp table
    observeEvent(
      c(input$forecast_date), {
        max_download_date <- NHSRtt::latest_rtt_date()
        min_download_date <- lubridate::floor_date(
          max_download_date,
          unit = "months"
        ) %m-% months(11)

        r$period_lkp <- dplyr::tibble(
          period = seq(
            from = min_download_date,
            to = forecast_dates()$end,
            by = "months"
          )
        ) |>
          mutate(
            period_id = dplyr::row_number()
          )
      },
      ignoreInit = TRUE
    )


    observeEvent(
      input$dwnld_rtt_data, {

        max_download_date <- NHSRtt::latest_rtt_date()
        min_download_date <- lubridate::floor_date(
          max_download_date,
          unit = "months"
        ) %m-% months(11)

        r$all_data <- NHSRtt::get_rtt_data(
          date_start = min_download_date,
          date_end = max_download_date,
          # trust_parent_codes = input$trust_parent_codes,
          trust_codes = names(trust_lkp[trust_lkp %in% input$trust_codes]),
          # commissioner_parent_codes = input$commissioner_parent_codes,
          # commissioner_org_codes = input$commissioner_org_codes,
          specialty_codes = specialty_lkp |>
            filter(Treatment.Function.Name %in% input$specialty_codes) |>
            pull(Treatment.Function.Code)
        ) |>
          summarise(
            value = sum(value),
            .by = c(
              trust, specialty, period, months_waited, type
            )
          ) |>
          mutate(
            months_waited_id = NHSRtt::convert_months_waited_to_id(
              months_waited,
              12 # this pools the data at 12+ months (this can be a user input in the future)
            ),
            trust = stringr::str_replace_all(
              trust,
              trust_lkp
            ),
            specialty = stringr::str_replace_all(
              specialty,
              treatment_function_codes
            )
          ) |>
          summarise(
            value = sum(value),
            .by = c(
              trust,
              specialty,
              period,
              type,
              months_waited_id
            )
          ) |>
          arrange(
            trust,
            specialty,
            type,
            months_waited_id,
            period
          ) |>
          tidyr::complete(
            specialty = input$specialty_codes,
            type = c("Complete", "Incomplete"),
            months_waited_id,
            period = seq(
              from = min_download_date,
              to = lubridate::floor_date(max_download_date, unit = "months"),
              by = "months"
            ),
            trust,
            fill = list(value = 0)
          ) |>
          tidyr::complete(
            specialty = input$specialty_codes,
            type = "Referrals",
            months_waited_id = 0,
            period = seq(
              from = min_download_date,
              to = lubridate::floor_date(max_download_date, unit = "months"),
              by = "months"
            ),
            trust,
            fill = list(value = 0)
          ) |>
          mutate(
            period_id = dplyr::row_number(), # we need period_id for later steps
            .by = c(
              trust,
              specialty,
              type,
              months_waited_id
            )
          )
      },
      ignoreInit = TRUE
    )

# dynamic UI --------------------------------------------------------------

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
          bslib::input_task_button(
            id = ns("optimise_capacity"),
            label = "Run capacity optimisation",
            label_busy = "Forecasting...",
            type = "secondary"
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
          bslib::input_task_button(
            id = ns("calculate_performance"),
            label = "Calculate future performance",
            label_busy = "Forecasting...",
            type = "secondary"
          )
          # actionButton(
          #   inputId = ns("calculate_performance"),
          #   label = "Calculate future performance",
          #
          # )
        )
      }
    })


# Forecast performance based on capacity inputs ---------------------------

    observeEvent(
      c(input$calculate_performance) , {

        params <- calibrate_parameters(
          r$all_data,
          max_months_waited = 12
        )

        forecast_months <- lubridate::interval(
          as.Date(input$forecast_date[[1]]),
          as.Date(input$forecast_date[[2]])
        ) %/% months(1)

        projections_referrals <- r$all_data |>
          filter(
            type == "Referrals"
          ) |>
          forecast_function(
            number_timesteps = forecast_months - 1,
            method = input$referral_growth_type,
            percent_change = input$referral_growth
          )

        projections_capacity <- r$all_data |>
          filter(
            type == "Complete"
          ) |>
          summarise(
            value = sum(value),
            .by = c(
              specialty, trust, type, period, period_id
            )
          ) |>
          forecast_function(
            number_timesteps = forecast_months - 1,
            method = input$capacity_growth_type,
            percent_change = input$capacity_growth
          )

        t0_incompletes <- r$all_data |>
          filter(
            type == "Incomplete",
            period == max(period)
          ) |>
          select(
            months_waited_id,
            incompletes = "value"
          )

        r$waiting_list <- NHSRtt::apply_params_to_projections(
          capacity_projections = projections_capacity,
          referrals_projections = projections_referrals,
          incomplete_pathways = t0_incompletes,
          renege_capacity_params = params$params[[1]],
          max_months_waited = 12
        ) |>
          mutate(
            period_id = period_id + max(r$all_data$period_id)
          ) |>
          left_join(
            r$period_lkp,
            by = join_by(
              period_id
            )
          )
      },
      ignoreInit = TRUE
    )
  })
}

## To be copied in the UI
# mod_02_planner_ui("02_planner_1")

## To be copied in the server
# mod_02_planner_server("02_planner_1")


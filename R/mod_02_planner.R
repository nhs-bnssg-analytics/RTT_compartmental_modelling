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
#'   bs_theme page_fluid card_body layout_columns
mod_02_planner_ui <- function(id){
  ns <- NS(id)

  filters_card <- card(
    card_header("Select filters on data"),
    card_body(
      min_height = 600,
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
  )

  scenario_card <- card(
    card_header("Select dates for analysis and forecasting"),
    card_body(
      class = "inline",
      uiOutput(
        ns("forecast_horizon")
      ),
      layout_columns(
        col_widths = c(4, 2),
        span("Percentage change in referrals (between -20% and 200%):"),
        numericInput(
          inputId = ns("referral_growth"),
          label = NULL,
          value = 0,
          min = -20,
          max = 200
        ),
        fill = FALSE
      ),
      layout_columns(
        col_widths = c(2, 3),
        span("Select type of referral change:"),
        radioButtons(
          inputId = ns("referral_growth_type"),
          label = NULL,
          choices = c("Uniform", "Linear"),
          selected = "Linear"#,
          # choiceNames = c("Uplift referrals uniformly", "Uplift referrals to change by a percentage (linearly) by the end of the time period"),
          # choiceValues = c("uniform", "linear")
        ),
        fill = FALSE
      ),
      layout_columns(
        col_widths = c(2, 3),
        span("Select scenario type:"),
        selectInput(
          inputId = ns("interface_choice"),
          label = NULL,
          choices = c(
            "Select..." = "select",
            "Estimate performance (from capacity inputs)" = "capacity_inputs",
            "Estimate capacity (from performance targets)" = "performance_inputs"
          ),
          multiple = FALSE
        ),
        fill = FALSE
      ),
      uiOutput(
        ns("dynamic_interface")
      )
    )
  )

  tagList(
    bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      bslib::layout_sidebar(
        sidebar = sidebar(
          filters_card,
          open = TRUE,
          width = '25%'
        ),
        scenario_card
      )
    )

  )
}

#' 02_planner Server Functions
#'
#' @importFrom shiny observeEvent renderUI dateInput tagList numericInput
#'   eventReactive
#' @importFrom shinyWidgets numericRangeInput
#' @importFrom NHSRtt get_rtt_data latest_rtt_date convert_months_waited_to_id
#'   apply_params_to_projections apply_parameter_skew optimise_capacity
#' @importFrom lubridate `%m+%` `%m-%` floor_date ceiling_date interval
#' @importFrom stringr str_replace_all
#' @importFrom dplyr mutate summarise arrange row_number cross_join left_join
#'   join_by bind_rows
#' @importFrom tidyr complete unnest
#' @importFrom purrr map2 map
#' @noRd
mod_02_planner_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    reactive_values <- reactiveValues()

    reactive_values$data_downloaded <- FALSE
    reactive_values$params <- NULL
    reactive_values$calibration_data <- NULL

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
        # ) %m-% months(11) # SWAP BACK FOR LIVE VERSION
        ) %m-% months(1)

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

        reactive_values$data_downloaded <- TRUE

        reactive_values$params <- calibrate_parameters(
          r$all_data,
          max_months_waited = 12
        )


        # data frame of counts by period which get supplied to the 3rd module
        # for charting
        reactive_values$calibration_data <- calibrate_parameters(
          r$all_data,
          max_months_waited = 12,
          full_breakdown = TRUE
        ) |>
          select(
            "params"
          ) |>
          tidyr::unnest(params) |>
          dplyr::select(
            "period_id",
            "months_waited_id",
            calculated_treatments = "treatments",
            "reneges",
            incompletes = "waiting_same_node"
          ) |>
          dplyr::mutate(
            capacity_skew = 1,
            period_type = "Observed"
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
      layout_columns(
        col_widths = c(2, 4),
        span("Forecast horizon date range:"),
        dateRangeInput(
          inputId = ns("forecast_date"),
          label = NULL,
          min = "2016-05-01",
          start = forecast_dates()$start,
          end = forecast_dates()$end
        ),
        fill = FALSE
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
      layout_columns(
        col_widths = c(2, 4),
        span("Select date to achieve target by:"),
        dateInput(
          inputId = ns("target_achievement_date"),
          label = NULL,
          min = target_dates()$min,
          max =  target_dates()$max,
          value = target_dates()$max
        ),
        fill = FALSE
      )
    )


# make buttons appear if the data has already been downloaded
    output$optimise_capacity_ui <- renderUI({
      if (isTRUE(reactive_values$data_downloaded)) {
        bslib::input_task_button(
          id = ns("optimise_capacity"),
          label = "Run capacity optimisation",
          label_busy = "Forecasting...",
          type = "secondary"
        )
      }
    })

    output$calculate_performance_ui <- renderUI({
      if (isTRUE(reactive_values$data_downloaded)) {
        bslib::input_task_button(
          id = ns("calculate_performance"),
          label = "Calculate future performance",
          label_busy = "Forecasting...",
          type = "secondary"
        )
      }
    })

    # Generate the dynamic UI based on dropdown selection
    output$dynamic_interface <- renderUI({
      r$waiting_list <- dplyr::tibble()

      if (input$interface_choice == "select") {
        tagList()
      } else if (input$interface_choice == "performance_inputs") {
        # Numeric interface
        tagList(
          uiOutput(
            ns("target_achievement_date")
          ),
          layout_columns(
            col_widths = c(2, 2),
            span("Target percentage (between 0% and 100%):"),
            numericInput(
              # INPUT (note, the package requires the 100% - x of this value, eg, 65% performance = a target_value of 35%)
              inputId = ns("target_value"),
              label = NULL,
              min = 0,
              max = 100,
              value = 70
            ),
            fill = FALSE
          ),
          layout_columns(
            col_widths = c(2, 2),
            span("Select type of capacity change:"),
            radioButtons(
              inputId = ns("optimised_capacity_growth_type"),
              label = NULL,
              choices = c("Uniform", "Linear"),
              selected = "Linear"#,
              # choiceNames = c("Uplift referrals uniformly", "Uplift referrals to change by a percentage (linearly) by the end of the time period"),
              # choiceValues = c("uniform", "linear")
            ),
            fill = FALSE
          ),
          layout_columns(
            col_widths = c(2, 3),
            span("Select range of capacity skews:"),
            numericRangeInput(
              inputId = ns("capacity_skew_range"),
              label = NULL,
              value = c(0.8, 1.2),
              min = 0.1,
              max = 3, #this is arbitrary
              step = 0.05
            ),
            fill = FALSE
          ),
          uiOutput(
            ns("optimise_capacity_ui")
          )
        )
      } else if (input$interface_choice == "capacity_inputs") {
        # Text interface
        tagList(
          layout_columns(
            col_widths = c(3, 1),
            span("Percentage change for capacity (between -20% and 20%):"),
            numericInput(
              inputId = ns("capacity_growth"),
              label = NULL,
              value = 0,
              min = -20,
              max = 200
            ),
            fill = FALSE
          ),
          layout_columns(
            col_widths = c(2, 2),
            span("Select type of capacity change:"),
            radioButtons(
              inputId = ns("capacity_growth_type"),
              label = NULL,
              choices = c("Uniform", "Linear"),
              selected = "Linear"#,
              # choiceNames = c("Uplift referrals uniformly", "Uplift referrals to change by a percentage (linearly) by the end of the time period"),
              # choiceValues = c("uniform", "linear")
            ),
            fill = FALSE
          ),
          layout_columns(
            col_widths = c(3, 2),
            span("Enter capacity utilisation skew:"),
            numericInput(
              inputId = ns("capacity_skew"),
              label = NULL,
              value = 1,
              min = 0.1,
              max = 3, #this is arbitrary
              step = 0.05
            ),
            fill = FALSE
          ),
          uiOutput(
            ns("calculate_performance_ui")
          )
        )
      }
    })


# Forecast performance based on capacity inputs ---------------------------

    observeEvent(
      c(input$calculate_performance) , {

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
          renege_capacity_params = reactive_values$params$params[[1]] |>
            mutate(
              capacity_param = NHSRtt::apply_parameter_skew(
                capacity_param,
                skew = input$capacity_skew
              )
            ),
          max_months_waited = 12
        ) |>
          mutate(
            period_id = period_id + max(r$all_data$period_id),
            capacity_skew = input$capacity_skew,
            period_type = "Projected"
          ) |>
          dplyr::bind_rows(
            reactive_values$calibration_data
          ) |>
          dplyr::arrange(
            period_id
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

    observeEvent(
      c(input$optimise_capacity), {

        if (isTRUE(reactive_values$data_downloaded)) {
          skew <- dplyr::tibble(
            skew_param = seq(
              from = min(input$capacity_skew_range),
              to = max(input$capacity_skew_range),
              by = 0.05
            )
          )

          forecast_months_to_target <- lubridate::interval(
            as.Date(input$forecast_date[[1]]),
            as.Date(input$target_achievement_date)
          ) %/% months(1)

          projections_referrals <- r$all_data |>
            filter(
              type == "Referrals"
            ) |>
            forecast_function(
              number_timesteps = forecast_months_to_target - 1,
              method = input$referral_growth_type,
              percent_change = input$referral_growth
            )

          t1_capacity <- r$all_data |>
            filter(
              type == "Complete"
            ) |>
            summarise(
              value = sum(value),
              .by = c(
                specialty, trust, type, period, period_id
              )
            ) |>
            calculate_t1_value()

          t0_incompletes <- r$all_data |>
            filter(
              type == "Incomplete",
              period == max(period)
            ) |>
            select(
              months_waited_id,
              incompletes = "value"
            )

          skewed_params <- reactive_values$params |>
            dplyr::cross_join(
              skew
            ) |>
            mutate(
              params = purrr::map2(
                .x = params,
                .y = skew_param,
                \(x, y) x |>
                  mutate(
                    capacity_param = NHSRtt::apply_parameter_skew(
                      params = capacity_param,
                      skew = y
                    )
                  )
              )
            )

          if (input$optimised_capacity_growth_type == "Uniform") {
            cap_prof <- "flat"
          } else if (input$optimised_capacity_growth_type == "Linear") {
            cap_prof <- "linear_change"
          }

          # calculate optimised uplift
          min_uplift <- skewed_params |>
            mutate(
              uplift = purrr::map(
                .x = params,
                ~ optimise_capacity(
                  t_1_capacity = t1_capacity,
                  referrals_projections = projections_referrals,
                  incomplete_pathways = t0_incompletes,
                  renege_capacity_params = .x,
                  target = paste0(1 - (input$target_value / 100), "%"),
                  target_bin = 4,
                  capacity_profile = cap_prof,
                  tolerance = 0.001,
                  max_iterations = 35
                )
              ),
              status = names(unlist(uplift)),
              uplift = as.numeric(uplift)
            ) |>
            filter(
              uplift == min(uplift)
            )

          # forecast future waiting list based on uplifted numbers

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
              method = input$optimised_capacity_growth_type,
              percent_change = (min_uplift$uplift - 1) * 100 # convert the uplift value into a percent
            )

          r$waiting_list <- NHSRtt::apply_params_to_projections(
            capacity_projections = projections_capacity,
            referrals_projections = projections_referrals,
            incomplete_pathways = t0_incompletes,
            renege_capacity_params = min_uplift$params[[1]],
            max_months_waited = 12
          ) |>
            dplyr::mutate(
              period_id = period_id + max(r$all_data$period_id),
              capacity_skew = min_uplift$skew_param,
              period_type = "Projected"
            ) |>
            dplyr::bind_rows(
              reactive_values$calibration_data
            ) |>
            dplyr::arrange(
              period_id
            ) |>
            dplyr::left_join(
              r$period_lkp,
              by = join_by(
                period_id
              )
            )
        }
      }
    )

  })
}



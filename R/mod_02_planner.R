#' 02_planner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput radioButtons numericInput
#'   dateRangeInput dateInput selectInput icon
#' @importFrom bslib input_task_button card card_header layout_sidebar sidebar
#'   bs_theme page_fluid card_body layout_columns tooltip
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
        col_widths = c(3, 4),
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
        col_widths = c(3, 4),
        span(
          "Select type of referral change:",
          tooltip(
            shiny::icon("info-circle"),
            "Mass measured in grams.",
            placement = "right"
          )
        ),
        radioButtons(
          inputId = ns("referral_growth_type"),
          label = NULL,
          choices = c("Uniform", "Linear"),
          selected = "Linear"#,
          # choiceNames = c("Uplift referrals uniformly", "Uplift referrals to change by a percentage (linearly) by the end of the time period"),
          # choiceValues = c("uniform", "linear")
        ),
        fill = FALSE
      )
    ),
    # Horizontal divider line
    hr(),
    card_body(
      layout_columns(
        col_widths = c(3, 4),
        span(
          "Select scenario type:",
          tooltip(
            shiny::icon("info-circle"),
            "Mass measured in grams.",
            placement = "right"
          )
        ),
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
    ),
    fill = FALSE,
    min_height = 650
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
        scenario_card,
        fill = FALSE,
        fillable = FALSE
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
#' @importFrom bslib tooltip
#' @importFrom rlang .data
#' @noRd
mod_02_planner_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    reactive_values <- reactiveValues()

    reactive_values$data_downloaded <- FALSE
    reactive_values$params <- NULL
    reactive_values$calibration_data <- NULL
    reactive_values$latest_performance <- NULL

    # create period_lkp table from the first time period in the calibration data
    # to the final time period in the projection period
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
            filter(.data$Treatment.Function.Name %in% input$specialty_codes) |>
            pull(.data$Treatment.Function.Code)
        ) |>
          summarise(
            value = sum(.data$value),
            .by = c(
              "trust", "specialty", "period", "months_waited", "type"
            )
          ) |>
          mutate(
            months_waited_id = NHSRtt::convert_months_waited_to_id(
              .data$months_waited,
              12 # this pools the data at 12+ months (this can be a user input in the future)
            ),
            trust = stringr::str_replace_all(
              .data$trust,
              trust_lkp
            ),
            specialty = stringr::str_replace_all(
              .data$specialty,
              treatment_function_codes
            )
          ) |>
          summarise(
            value = sum(.data$value),
            .by = c(
              "trust",
              "specialty",
              "period",
              "type",
              "months_waited_id"
            )
          ) |>
          arrange(
            .data$trust,
            .data$specialty,
            .data$type,
            .data$months_waited_id,
            .data$period
          ) |>
          tidyr::complete(
            specialty = input$specialty_codes,
            type = c("Complete", "Incomplete"),
            .data$months_waited_id,
            period = seq(
              from = min_download_date,
              to = lubridate::floor_date(max_download_date, unit = "months"),
              by = "months"
            ),
            .data$trust,
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
            .data$trust,
            fill = list(value = 0)
          ) |>
          mutate(
            period_id = dplyr::row_number(), # we need period_id for later steps
            .by = c(
              .data$trust,
              .data$specialty,
              .data$type,
              .data$months_waited_id
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
          tidyr::unnest(.data$params) |>
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

        reactive_values$latest_performance <- r$all_data |>
          filter(
            .data$type == "Incomplete",
            .data$period == max(.data$period)
          ) |>
          calc_performance(
            target_bin = 4
          ) |>
          mutate(
            text = paste0(
              "The performance at ",
              format(.data$period, '%b %y'),
              " was ",
              format(
                100 * .data$prop,
                format = "f",
                digits = 2,
                nsmall = 1
              ),
              "%"
            )
          ) |>
          pull(.data$text)

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
        col_widths = c(3, 4),
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
        col_widths = c(3, 4),
        span(
          "Select date to achieve target by:",
          tooltip(
            shiny::icon("info-circle"),
            "Mass measured in grams.",
            placement = "right"
          )
        ),
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

    output$latest_performance_ui <- shiny::renderUI({
      if (is.null(reactive_values$latest_performance)) {
        return(NULL)
      } else {
        div(
          p(
            # class = "display-5 text-primary",
            reactive_values$latest_performance
          )
        )
      }

    }

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
          uiOutput(
            ns("latest_performance")
          ),
          uiOutput(
            ns("latest_performance_ui")
          ),
          layout_columns(
            col_widths = c(3, 4),
            span(
              "Target percentage (between 0% and 100%):",
              tooltip(
                shiny::icon("info-circle"),
                "Mass measured in grams.",
                placement = "right"
              )
            ),
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
            col_widths = c(3, 4),
            span(
              "Select type of capacity change:",
              tooltip(
                shiny::icon("info-circle"),
                "Mass measured in grams.",
                placement = "right"
              )
            ),
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
            col_widths = c(3, 4),
            span(
              "Select range of capacity skews:",
              tooltip(
                shiny::icon("info-circle"),
                "Mass measured in grams.",
                placement = "right"
              )
            ),
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
            col_widths = c(3, 4),
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
            col_widths = c(3, 4),
            span(
              "Select type of capacity change:",
              tooltip(
                shiny::icon("info-circle"),
                "Mass measured in grams.",
                placement = "right"
              )
            ),
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
            col_widths = c(3, 4),
            span(
              "Enter capacity utilisation skew:",
              tooltip(
                shiny::icon("info-circle"),
                "Mass measured in grams.",
                placement = "right"
              )
            ),
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
            .data$type == "Referrals"
          ) |>
          forecast_function(
            number_timesteps = forecast_months - 1,
            method = input$referral_growth_type,
            percent_change = input$referral_growth
          )

        projections_capacity <- r$all_data |>
          filter(
            .data$type == "Complete"
          ) |>
          summarise(
            value = sum(.data$value),
            .by = c(
              "specialty", "trust", "type", "period", "period_id"
            )
          ) |>
          forecast_function(
            number_timesteps = forecast_months - 1,
            method = input$capacity_growth_type,
            percent_change = input$capacity_growth
          )

        t0_incompletes <- r$all_data |>
          filter(
            .data$type == "Incomplete",
            .data$period == max(.data$period)
          ) |>
          select(
            "months_waited_id",
            incompletes = "value"
          )

        r$waiting_list <- NHSRtt::apply_params_to_projections(
          capacity_projections = projections_capacity,
          referrals_projections = projections_referrals,
          incomplete_pathways = t0_incompletes,
          renege_capacity_params = reactive_values$params$params[[1]] |>
            mutate(
              capacity_param = NHSRtt::apply_parameter_skew(
                .data$capacity_param,
                skew = input$capacity_skew
              )
            ),
          max_months_waited = 12
        ) |>
          mutate(
            period_id = .data$period_id + max(r$all_data$period_id),
            capacity_skew = input$capacity_skew,
            period_type = "Projected"
          ) |>
          dplyr::bind_rows(
            reactive_values$calibration_data
          ) |>
          dplyr::arrange(
            .data$period_id
          ) |>
          left_join(
            r$period_lkp,
            by = join_by(
              .data$period_id
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
              .data$type == "Referrals"
            ) |>
            forecast_function(
              number_timesteps = forecast_months_to_target - 1,
              method = input$referral_growth_type,
              percent_change = input$referral_growth
            )

          t1_capacity <- r$all_data |>
            filter(
              .data$type == "Complete"
            ) |>
            summarise(
              value = sum(.data$value),
              .by = c(
                "specialty", "trust", "type", "period", "period_id"
              )
            ) |>
            calculate_t1_value()

          t0_incompletes <- r$all_data |>
            filter(
              .data$type == "Incomplete",
              .data$period == max(.data$period)
            ) |>
            select(
              "months_waited_id",
              incompletes = "value"
            )

          skewed_params <- reactive_values$params |>
            dplyr::cross_join(
              skew
            ) |>
            mutate(
              params = purrr::map2(
                .x = .data$params,
                .y = .data$skew_param,
                \(x, y) x |>
                  mutate(
                    capacity_param = NHSRtt::apply_parameter_skew(
                      params = .data$capacity_param,
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
                .x = .data$params,
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
              status = names(unlist(.data$uplift)),
              uplift = as.numeric(.data$uplift)
            ) |>
            filter(
              .data$uplift == min(.data$uplift)
            )

          # forecast future waiting list based on uplifted numbers

          forecast_months <- lubridate::interval(
            as.Date(input$forecast_date[[1]]),
            as.Date(input$forecast_date[[2]])
          ) %/% months(1)

          projections_referrals <- r$all_data |>
            filter(
              .data$type == "Referrals"
            ) |>
            forecast_function(
              number_timesteps = forecast_months - 1,
              method = input$referral_growth_type,
              percent_change = input$referral_growth
            )

          projections_capacity <- r$all_data |>
            filter(
              .data$type == "Complete"
            ) |>
            summarise(
              value = sum(.data$value),
              .by = c(
                "specialty", "trust", "type", "period", "period_id"
              )
            ) |>
            forecast_function(
              number_timesteps = forecast_months - 1,
              method = input$optimised_capacity_growth_type,
              percent_change = (.data$min_uplift$uplift - 1) * 100 # convert the uplift value into a percent
            )

          r$waiting_list <- NHSRtt::apply_params_to_projections(
            capacity_projections = projections_capacity,
            referrals_projections = projections_referrals,
            incomplete_pathways = t0_incompletes,
            renege_capacity_params = min_uplift$params[[1]],
            max_months_waited = 12
          ) |>
            dplyr::mutate(
              period_id = .data$period_id + max(r$all_data$period_id),
              capacity_skew = min_uplift$skew_param,
              period_type = "Projected"
            ) |>
            dplyr::bind_rows(
              reactive_values$calibration_data
            ) |>
            dplyr::arrange(
              .data$period_id
            ) |>
            dplyr::left_join(
              r$period_lkp,
              by = join_by(
                .data$period_id
              )
            )
        }
      }
    )

  })
}



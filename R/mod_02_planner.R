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

  filters_sidebar <- sidebar(

    # card_header("Select filters on data"),
    open = TRUE,
    width = '35%',

    selectizeInput(
      inputId = ns("region"),
      label = "Select NHS Region(s)",
      choices = sort(unique(org_lkp$`NHS Region Name`)),
      options = list(
        placeholder = "Leave blank to aggregate all available regions"
      ),
      multiple = TRUE
    ),
    selectizeInput(
      inputId = ns("trust_parent_codes"),
      label = "Select Provider Parent(s)",
      choices = sort(unique(org_lkp$`Provider Parent Name`)),
      options = list(
        placeholder = "Leave blank to aggregate all available provider parent orgs"
      ),
      multiple = TRUE
    ),
    selectizeInput(
      inputId = ns("commissioner_parent_codes"),
      label = "Select Commissioner Parent(s)",
      choices = sort(unique(org_lkp$`Commissioner Parent Name`)),
      options = list(
        placeholder = "Leave blank to aggregate all available commissioner parent orgs"
      ),
      multiple = TRUE
    ),
    selectizeInput(
      inputId = ns("commissioner_org_codes"),
      label = "Select Commissioner Org(s)",
      choices = sort(unique(org_lkp$`Commissioner Org Name`)),
      options = list(
        placeholder = "Leave blank to aggregate all available commissioner orgs"
      ),
      multiple = TRUE
    ),
    selectizeInput(
      inputId = ns("trust_codes"),
      label = "Select Provider(s)",
      choices = sort(unique(org_lkp$`Provider Org Name`)),
      options = list(
        placeholder = "Leave blank to aggregate all available providers"
      ),
      multiple = TRUE
    ),
    selectInput(
      inputId = ns("specialty_codes"),
      label = "Select Specialties",
      choices = unname(treatment_function_codes),
      multiple = FALSE
    ),
    checkboxInput(
      inputId = ns("nhs_only"),
      label = "Show NHS providers only",
      value = TRUE,
    ),
    sliderInput(
      inputId = ns("calibration_months"),
      label = "Select number of months to calibrate data on:",
      min = 2,
      max = 24,
      value = 12
    ),
    bslib::input_task_button(
      id = ns("dwnld_rtt_data"),
      label = "Download RTT data",
      label_busy = "Downloading...",
      type = "secondary"
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
            shiny::HTML(
              paste0(
                "<strong>Uniform:</strong> ",
                "The change in referral counts occurs in the first month and remains flat for the whole 'Forecast horizon' period.<br><br>",
                "<strong>Linear:</strong> ",
                "The first month of the 'Forecast horizon' period is estimated from the historic data, and then referral counts are changed linearly until the end of the 'Forecast horizon'."
              )
            ),
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
            shiny::HTML(
              paste0(
                "Option 1: see the impact of <strong>providing future capacity inputs</strong> on waiting lists and performance. <br><br>",
                "Option 2: calculate the optimal capacity to achieve a <strong>provided performance input</strong>."
              )
            ),
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
        sidebar = filters_sidebar,
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
#'   eventReactive Progress sliderInput HTML
#' @importFrom NHSRtt get_rtt_data latest_rtt_date convert_months_waited_to_id
#'   apply_params_to_projections apply_parameter_skew optimise_capacity
#' @importFrom lubridate `%m+%` `%m-%` floor_date ceiling_date interval
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
    reactive_values$default_target <- NULL
    reactive_values$referrals_uplift <- NULL
    reactive_values$optimise_status_card_visible <- NULL

    r$chart_specification <- list(
      trust = NULL,
      specialty = NULL,
      observed_start = NULL,
      observed_end = NULL,
      forecast_start = NULL,
      forecast_end = NULL,
      referrals_percent_change = NULL,
      referrals_change_type = NULL,
      scenario_type = NULL,
      capacity_percent_change = NULL,
      capacity_change_type = NULL,
      capacity_skew = NULL,
      target_date = NULL,
      target_performance = NULL,
      optimise_status = NULL
    )


# area selection filtering based on other selections ----------------------
    data_table <- reactiveVal(org_lkp)

    observeEvent(
      c(input$region,
        input$trust_parent_codes,
        input$commissioner_parent_codes,
        input$commissioner_org_codes,
        input$trust_codes,
        input$nhs_only
      ), {

        data_table <- org_lkp

        if (isTRUE(input$nhs_only)) {
          data_table <- data_table |>
            dplyr::filter(
              grepl("NHS", .data$`Provider Org Name`)
            )
        }

        if (length(input$region) > 0) {
          data_table <- data_table |>
            dplyr::filter(
              .data$`NHS Region Name` %in% input$region
            )
        }

        if (length(input$trust_parent_codes) > 0) {
          data_table <- data_table |>
            dplyr::filter(
              .data$`Provider Parent Name` %in% input$trust_parent_codes
            )
        }

        if (length(input$commissioner_parent_codes) > 0) {
          data_table <- data_table |>
            dplyr::filter(
              .data$`Commissioner Parent Name` %in% input$commissioner_parent_codes
            )
        }

        if (length(input$commissioner_org_codes) > 0) {
          data_table <- data_table |>
            dplyr::filter(
              .data$`Commissioner Org Name` %in% input$commissioner_org_codes
            )
        }

        if (length(input$trust_codes) > 0) {
          data_table <- data_table |>
            dplyr::filter(
              .data$`Provider Org Name` %in% input$trust_codes
            )
        }

        # provider_parent current selections
        current_provider_parent <- dplyr::intersect(
          input$trust_parent_codes,
          unique(data_table[["Provider Parent Name"]])
        )
        # if (is.null(current_provider_parent)) current_provider_parent <- "All"

        updateSelectizeInput(
          session,
          inputId = "trust_parent_codes",
          choices = sort(unique(data_table[["Provider Parent Name"]])),
          selected = current_provider_parent
        )


        # commissioner_parent current selections
        current_commissioner_parent <- dplyr::intersect(
          input$commissioner_parent_codes,
          unique(data_table[["Commissioner Parent Name"]])
        )

        # if (is.null(current_commissioner_parent)) current_commissioner_parent <- "All"
        updateSelectizeInput(
          session,
          inputId = "commissioner_parent_codes",
          choices = sort(unique(data_table[["Commissioner Parent Name"]])),
          selected = current_commissioner_parent
        )

        # commissioner_org current selections
        current_commissioner_org <- dplyr::intersect(
          input$commissioner_org_codes,
          unique(data_table[["Commissioner Org Name"]])
        )

        # if (is.null(current_commissioner_org)) current_commissioner_org <- "All"
        updateSelectizeInput(
          session,
          inputId = "commissioner_org_codes",
          choices = sort(unique(data_table[["Commissioner Org Name"]])),
          selected = current_commissioner_org
        )

        # provider current selections
        current_provider <- dplyr::intersect(
          input$trust_codes,
          unique(data_table[["Provider Org Name"]])
        )

        # if (is.null(current_provider)) current_provider <- "All"
        updateSelectizeInput(
          session,
          inputId = "trust_codes",
          choices = sort(unique(data_table[["Provider Org Name"]])),
          selected = current_provider
        )
      })

# download data button ----------------------------------------------------
    observeEvent(
      input$dwnld_rtt_data, {

        max_download_date <- NHSRtt::latest_rtt_date()
        min_download_date <- lubridate::floor_date(
          max_download_date,
          unit = "months"
        ) %m-% months(input$calibration_months)

        # create period_lkp table from the first time period in the calibration data
        # to the final time period in the projection period
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

        # create progress bar
        progress <- Progress$new(
          session,
          min = 1,
          max = input$calibration_months + 1
        )

        on.exit(progress$close())
        progress$set(message = 'Downloading public data from RTT statistics',
                     detail = 'This is used for calibrating the model')

        selections_labels <- filters_displays(
          trust_parents = input$trust_parent_codes,
          trusts = input$trust_codes,
          comm_parents = input$commissioner_parent_codes,
          comms = input$commissioner_org_codes,
          spec = specialty_lkp |>
            filter(.data$Treatment.Function.Name %in% input$specialty_codes) |>
            pull(.data$Treatment.Function.Code)
        )
# browser()
        # pass some values to the charting module
        r$chart_specification$trust <- selections_labels$trusts$selected_name
        r$chart_specification$specialty <- selections_labels$specialties$selected_name
        r$chart_specification$observed_start <- min_download_date
        r$chart_specification$observed_end <- max_download_date

        # download and aggregate data
        r$all_data <- get_rtt_data_with_progress(
          date_start = min_download_date,
          date_end = max_download_date,
          trust_parent_codes = selections_labels$trust_parents$selected_code,
          trust_codes = selections_labels$trusts$selected_code,
          commissioner_parent_codes = selections_labels$commissioner_parents$selected_code,
          commissioner_org_codes = selections_labels$commissioners$selected_code,
          specialty_codes = selections_labels$specialties$selected_code,
          progress = progress
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
            )
          )

        if (selections_labels$trusts$display == "Aggregated") {
          r$all_data <- r$all_data |>
            mutate(
              trust = "Aggregated"
            )
        } else {
          r$all_data <- r$all_data |>
            mutate(
              trust = replace_fun(
                .data$trust,
                trust_lkp
              )
            )
        }

        if (selections_labels$specialties$display == "Aggregated") {
          r$all_data <- r$all_data |>
            mutate(
              specialty = "Aggregated"
            )
        } else {
          r$all_data <- r$all_data |>
            mutate(
              specialty = replace_fun(
                .data$specialty,
                treatment_function_codes
              )
            )
        }

        r$all_data <- r$all_data |>
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

        # calculate unadjusted referrals
        unadjusted_referrals <- r$all_data |>
          filter(
            .data$type == "Referrals"
          ) |>
          dplyr::select(
            "period_id",
            "months_waited_id",
            unadjusted_referrals = "value"
          )

        # calculate the referrals uplift value by calibrating the parameters
        # with redistribute_m0_reneges set to FALSE
        reactive_values$referrals_uplift <- calibrate_parameters(
          r$all_data,
          max_months_waited = 12,
          redistribute_m0_reneges = FALSE,
          referrals_uplift = NULL
        ) |>
          tidyr::unnest(.data$params) |>
          dplyr::filter(
            .data$months_waited_id == 0
          ) |>
          dplyr::pull(.data$renege_param)

        # check whether the referrals uplift value is negative
        if (reactive_values$referrals_uplift < 0) {
          reactive_values$referrals_uplift <- abs(reactive_values$referrals_uplift)
        } else {
          reactive_values$referrals_uplift <- NULL
        }

        # calculate the modelling parameters using the uplifted referrals
        reactive_values$params <- calibrate_parameters(
          r$all_data,
          max_months_waited = 12,
          redistribute_m0_reneges = FALSE,
          referrals_uplift = reactive_values$referrals_uplift
        )

        # data frame of counts by period which get supplied to the 3rd module
        # for charting
        reactive_values$calibration_data <- calibrate_parameters(
          r$all_data,
          max_months_waited = 12,
          full_breakdown = TRUE,
          referrals_uplift = reactive_values$referrals_uplift,
          redistribute_m0_reneges = FALSE
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
          left_join(
            unadjusted_referrals,
            by = join_by(
              period_id, months_waited_id
            )
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

        reactive_values$default_target <- min(
          extract_percent(reactive_values$latest_performance) + 5,
          100
        )

        reactive_values$optimise_status_card_visible <- FALSE

      },
      ignoreInit = TRUE
    )

# dynamic UI --------------------------------------------------------------

    # data selectors

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

      if (dplyr::between(as.Date("2026-03-01"), min_date, max_date)) {
        default_date <- as.Date("2026-03-01")
      } else {
        default_date <- max_date
      }

      target_dates <- list(
        min = min_date,
        max = max_date,
        default = default_date
      )
    })


    output$target_achievement_date <- shiny::renderUI(
      layout_columns(
        col_widths = c(3, 4),
        span(
          "Select date to achieve target by:",
          tooltip(
            shiny::icon("info-circle"),
            "Restricted to the 'Forecast horizon date range' this is the date that the optimiser will use to achieve the 'Target percentage' on",
            placement = "right"
          )
        ),
        dateInput(
          inputId = ns("target_achievement_date"),
          label = NULL,
          min = target_dates()$min,
          max =  target_dates()$max,
          value = target_dates()$default
        ),
        fill = FALSE
      )
    )

    # the latest perforamnce value to be displayed
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

    })

# make scenario buttons appear if the data has already been downloaded
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

    output$optimisation_results_ui <- renderUI({

      if (isTRUE(reactive_values$optimise_status_card_visible)) {
        if (r$chart_specification$optimise_status == "waitlist_cleared") {
          val <- "Waitlist cleared"
          icn <- shiny::icon("circle-xmark")
          thm <- "red"
        } else if (r$chart_specification$optimise_status == "converged") {
          val <- "Optimisation successful"
          icn <- shiny::icon("clipboard-check")
          thm <- "green"
        } else {
          val <- r$chart_specification$optimise_status
          icn <- shiny::icon("question")
          thm <- "yellow"
        }

        bslib::value_box(
          title = "Optimisation status",
          value = val,
          showcase = icn,
          theme = thm
        )
      }
    })


# dynamic UI based on the scenario choice ---------------------------------

    # Generate the dynamic UI based on dropdown selection
    output$dynamic_interface <- renderUI({
      r$waiting_list <- dplyr::tibble()

      skew_settings <- tagList(
        layout_columns(
          col_widths = c(3, 4),
          span(
            "Select stock to pivot on:",
            tooltip(
              shiny::icon("info-circle"),
              "All skewing functions have a pivot point, which is a 'waiting stock' around which the skew occurs.",
              placement = "right"
            )
          ),
          sliderInput(
            inputId = ns("pivot_bin"),
            label = NULL,
            min = 2,
            max = 12,
            value = 4
          ),
          fill = FALSE
        ),
        layout_columns(
          col_widths = c(3, 4),
          span(
            "Choose skew method:",
            tooltip(
              shiny::icon("info-circle"),
              shiny::HTML(
                paste0(
                  "Option 1: <strong>rotate</strong> the capacity utilisation around the pivot point. <br><br>",
                  "Option 2: change the clock stop rates above the pivot point <strong>uniformly</strong>, and change the clock stop rates below the pivot point <strong>uniformly</strong> in the opposite direction."
                )
              ),
              placement = "right"
            )
          ),
          radioButtons(
            inputId = ns("skew_method"),
            label = NULL,
            choices = c(
              "Rotate" = "rotate",
              "Uniform" = "uniform"
            )
          ),
          fill = FALSE
        )
      )

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
                "The proportion of people on the RTT waiting list that have been waiting for less than four months",
                placement = "right"
              )
            ),
            numericInput(
              # INPUT (note, the package requires the 100% - x of this value, eg, 65% performance = a target_value of 35%)
              inputId = ns("target_value"),
              label = NULL,
              min = 0,
              max = 100,
              value = reactive_values$default_target
            ),
            fill = FALSE
          ),
          layout_columns(
            col_widths = c(3, 4),
            span(
              "Select type of capacity change:",
              tooltip(
                shiny::icon("info-circle"),
                shiny::HTML(
                  paste0(
                    "<strong>Uniform:</strong> ",
                    "Capacity change occurs in first month and remains flat for the whole 'Forecast horizon' period.<br><br>",
                    "<strong>Linear:</strong> ",
                    "The first month of the 'Forecast horizon' period is estimated from the historic data, and then capacity is changed linearly until the end of the 'Forecast horizon'."
                  )
                ),
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
                shiny::HTML(
                  paste0(
                    "A skew of 1 causes the profile of clock stop rates across the number of months waiting to be unchanged from the calibration period.<br><br>",
                    "A skew of greater than 1 will increase the clock stop rate for the longer waiters, and decrease the clock stop rate for the shorter waiters.<br><br>",
                    "A skew of less than 1 will decrease the clock stop rate for the longer waiters, and increase the clock stop rate for the shorter waiters.<br><br>",
                    "All skew values leave the first stock (e.g., for individuals waiting less than 1 month) unchanged from the clock stop rate calculated from the calibration period."
                  )
                ),
                placement = "right"
              )
            ),
            sliderInput(
              inputId = ns("capacity_skew_range"),
              label = NULL,
              value = c(0.8, 1.2),
              min = 0.1,
              max = 3, #this is arbitrary
              step = 0.05
            ),
            fill = FALSE
          ),
          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              title = "Advanced skew settings",
              layout_columns(
                col_widths = c(5, 5),
                skew_settings,
                plotOutput(
                  ns("skew_visual"),
                  click = "plot_click"
                )
              )
            )
          ),
          uiOutput(
            ns("optimise_capacity_ui")
          ),
          uiOutput(
            ns("optimisation_results_ui")
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
                shiny::HTML(
                  paste0(
                    "<strong>Uniform:</strong> ",
                    "Capacity change occurs in first month and remains flat for the whole 'Forecast horizon' period.<br><br>",
                    "<strong>Linear:</strong> ",
                    "The first month of the 'Forecast horizon' period is estimated from the historic data, and then capacity is changed linearly until the end of the 'Forecast horizon'."
                  )
                ),
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
                shiny::HTML(
                  paste0(
                    "A skew of 1 causes the profile of clock stop rates across the number of months waiting to be unchanged from the calibration period.<br><br>",
                    "A skew of greater than 1 will increase the clock stop rate for the longer waiters, and decrease the clock stop rate for the shorter waiters.<br><br>",
                    "A skew of less than 1 will decrease the clock stop rate for the longer waiting stocks, and increase the clock stop rate for the shorter waiters.<br><br>",
                    "All skew values leave the first stock (e.g., for individuals waiting less than 1 month) unchanged from the clock stop rate relative to the calibration period."
                  )
                ),
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
          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              title = "Advanced skew settings",
              layout_columns(
                col_widths = c(5, 5),
                skew_settings,
                plotOutput(
                  ns("skew_visual"),
                  click = "plot_click"
                )
              )
            )
          ),
          uiOutput(
            ns("calculate_performance_ui")
          )
        )
      }
    })


# change skew visual based on inputs --------------------------------------

    observeEvent(
      c(input$pivot_bin,
        input$skew_method,
        input$capacity_skew,
        input$capacity_skew_range), {

          if (input$interface_choice == "capacity_inputs") {
            skew_values <- input$capacity_skew
            if (!is.null(skew_values)) {
              # user can delete value before entering it again, which causes an error
              continue <- TRUE
            }
          } else if (input$interface_choice == "performance_inputs") {
            skew_values <- input$capacity_skew_range
            continue <- TRUE
          }

          if (continue <- TRUE) {
            output$skew_visual <- renderPlot({
              plot_skew(
                params = reactive_values$params$params[[1]],
                skew_values = skew_values,
                pivot_bin = input$pivot_bin,
                skew_method = input$skew_method
              )
            })
          }

        }
    )

# Forecast performance based on capacity inputs ---------------------------

    observeEvent(
      c(input$calculate_performance), {

        if (input$calculate_performance == 1) {
          forecast_months <- lubridate::interval(
            as.Date(input$forecast_date[[1]]),
            as.Date(input$forecast_date[[2]])
          ) %/% months(1)

          unadjusted_projections_referrals <- r$all_data |>
            filter(
              .data$type == "Referrals",
              # first period only used for the count of incompletes
              .data$period != min(.data$period)
            ) |>
            forecast_function(
              number_timesteps = forecast_months - 1,
              method = input$referral_growth_type,
              percent_change = input$referral_growth
            )

          if (!is.null(reactive_values$referrals_uplift)) {
            projections_referrals <- unadjusted_projections_referrals +
              (unadjusted_projections_referrals * reactive_values$referrals_uplift)
          } else {
            projections_referrals <- unadjusted_projections_referrals
          }

          projections_capacity <- r$all_data |>
            filter(
              .data$type == "Complete",
              # first period only used for the count of incompletes
              .data$period != min(.data$period)
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
                  skew = input$capacity_skew,
                  skew_method = input$skew_method,
                  pivot_bin = input$pivot_bin
                )
              ),
            max_months_waited = 12
          ) |>
            # add referrals onto data
            dplyr::left_join(
              dplyr::tibble(
                unadjusted_referrals = unadjusted_projections_referrals,
                months_waited_id = 0
              ) |>
                dplyr::mutate(
                  period_id = dplyr::row_number()
                ),
              by = join_by(
                period_id,
                months_waited_id
              )
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
                period_id
              )
            )

          # pass information to charting module
          r$chart_specification$forecast_start <- min(input$forecast_date)
          r$chart_specification$forecast_end <- max(input$forecast_date)
          r$chart_specification$referrals_percent_change <- input$referral_growth
          r$chart_specification$referrals_change_type <- input$referral_growth_type
          r$chart_specification$scenario_type <- "Estimate performance (from capacity inputs)"
          r$chart_specification$capacity_percent_change <- input$capacity_growth
          r$chart_specification$capacity_change_type <- input$capacity_growth_type
          r$chart_specification$capacity_skew <- input$capacity_skew
          r$chart_specification$target_date <- NA
          r$chart_specification$target_performance <- NA
          r$chart_specification$optimise_status <- NULL
        }
      },
      ignoreInit = TRUE
    )


# optimising forecast based on performance inputs -------------------------

    observeEvent(
      c(input$optimise_capacity), {

        if (input$optimise_capacity >= 1) {

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
          ) %/% months(1) + 1 # the plus 1 makes is inclusive of the final month

          projections_referrals <- r$all_data |>
            filter(
              .data$type == "Referrals"
            )

          if (!is.null(reactive_values$referrals_uplift)) {
            projections_referrals <- projections_referrals |>
              mutate(
                value = .data$value +
                  (.data$value * reactive_values$referrals_uplift)
              )
          }

          projections_referrals <- projections_referrals |>
            filter(
              # first period only used for the count of incompletes
              .data$period != min(.data$period)
            ) |>
            forecast_function(
              number_timesteps = forecast_months_to_target,
              method = input$referral_growth_type,
              percent_change = input$referral_growth
            )

          t1_capacity <- r$all_data |>
            filter(
              .data$type == "Complete",
              # first period only used for the count of incompletes
              .data$period != min(.data$period)
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
                      skew = y,
                      skew_method = input$skew_method,
                      pivot_bin = input$pivot_bin
                    )
                  )
              )
            )

          if (input$optimised_capacity_growth_type == "Uniform") {
            cap_prof <- "flat"
          } else if (input$optimised_capacity_growth_type == "Linear") {
            cap_prof <- "linear_change"
          }

          progress <- Progress$new(
            session,
            min = 1,
            max = nrow(skewed_params))
          on.exit(progress$close())

          progress$set(
            message = 'Calculating capacity change based on range of skews provided',
            detail = 'This may take a while...'
          )

          # calculate optimised uplift
          min_uplift <- skewed_params |>
            mutate(
              rowid = dplyr::row_number(),
              uplift = purrr::map2(
                .x = .data$params,
                .y = .data$rowid,
                \(x, y) {
                  progress$set(value = y)
                  optimise_capacity(
                    t_1_capacity = t1_capacity,
                    referrals_projections = projections_referrals,
                    incomplete_pathways = t0_incompletes,
                    renege_capacity_params = x,
                    target = paste0(100 - input$target_value, "%"),
                    target_bin = 4,
                    capacity_profile = cap_prof,
                    tolerance = 0.001,
                    max_iterations = 35
                  )
                }
              ),
              status = names(unlist(.data$uplift)),
              uplift = as.numeric(.data$uplift)
            ) |>
            filter(
              .data$uplift == min(.data$uplift)
            ) |>
            filter(
              # if there are multiple records that have the same capacity
              # uplift, select the record that has the smallest change from the
              # calibrated period's capacity utilisation profile (eg, the one
              # closest to 1)
              abs(.data$skew_param - 1) == min(abs(.data$skew_param - 1))
            )

          r$chart_specification$optimise_status <- min_uplift$status

          # forecast future waiting list based on uplifted numbers

          forecast_months <- lubridate::interval(
            as.Date(input$forecast_date[[1]]),
            as.Date(input$forecast_date[[2]])
          ) %/% months(1) + 1 # the plus 1 makes is inclusive of the final month

          unadjusted_projections_referrals <- r$all_data |>
            filter(
              .data$type == "Referrals",
              # first period only used for the count of incompletes
              .data$period != min(.data$period)
            ) |>
            forecast_function(
              number_timesteps = forecast_months,
              method = input$referral_growth_type,
              percent_change = input$referral_growth
            )

          if (!is.null(reactive_values$referrals_uplift)) {
            projections_referrals <- unadjusted_projections_referrals +
              (unadjusted_projections_referrals * reactive_values$referrals_uplift)
          } else {
            projections_referrals <- unadjusted_projections_referrals
          }

          projections_capacity <- r$all_data |>
            filter(
              .data$type == "Complete",
              # first period only used for the count of incompletes
              .data$period != min(.data$period)
            ) |>
            summarise(
              value = sum(.data$value),
              .by = c(
                "specialty", "trust", "type", "period", "period_id"
              )
            ) |>
            forecast_function(
              number_timesteps = forecast_months,
              method = input$optimised_capacity_growth_type,
              percent_change = (min_uplift$uplift - 1) * 100 # convert the uplift value into a percent
            ) |>
            # make negative capacity = 0
            (\(x) ifelse(x < 0, 0, x))()

          r$waiting_list <- NHSRtt::apply_params_to_projections(
            capacity_projections = projections_capacity,
            referrals_projections = projections_referrals,
            incomplete_pathways = t0_incompletes,
            renege_capacity_params = min_uplift$params[[1]],
            max_months_waited = 12
          ) |>
            # add referrals onto data
            dplyr::left_join(
              dplyr::tibble(
                unadjusted_referrals = unadjusted_projections_referrals,
                months_waited_id = 0
              ) |>
                dplyr::mutate(
                  period_id = dplyr::row_number()
                ),
              by = join_by(
                period_id,
                months_waited_id
              )
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
                period_id
              )
            )

          # pass information to charting module
          r$chart_specification$forecast_start <- min(input$forecast_date)
          r$chart_specification$forecast_end <- max(input$forecast_date)
          r$chart_specification$referrals_percent_change <- input$referral_growth
          r$chart_specification$referrals_change_type <- input$referral_growth_type
          r$chart_specification$scenario_type <- "Estimate capacity (from performance targets)"
          r$chart_specification$capacity_percent_change <- paste0(
            format(
              (min_uplift$uplift - 1) * 100,
              format = "f",
              digits = 2,
              nsmall = 1
            ),
            "%"
          )
          r$chart_specification$capacity_change_type <- input$optimised_capacity_growth_type
          r$chart_specification$capacity_skew <- min_uplift$skew_param[[1]]
          r$chart_specification$target_date <- input$target_achievement_date
          r$chart_specification$target_performance <- input$target_value

          reactive_values$optimise_status_card_visible <- TRUE

        }
      }
    )

  })
}



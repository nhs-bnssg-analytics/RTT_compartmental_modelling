#' 02_planner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput radioButtons numericInput
#'   dateRangeInput dateInput selectInput icon downloadLink downloadButton hr br
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
      selected = "Total",
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
      type = "dark"
    ),
    card(
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          title = "Upload your own data...",
          p("Your CSV file must contain these columns:"),
          tags$ul(
            tags$li(strong("period"), "- date; the first day of each month the data represent"),
            tags$li(strong("type"), "- accepted values: Referrals, Incomplete, Complete"),
            tags$li(strong("months_waited_id"), "- integers (0 to 12); the compartments waited
                    ('0' is the number of people waiting 0-1 months, and '12' is the number of people waiting 12+ months)"),
            tags$li(strong("value"), "- the counts for each compartment")
          ),
          p("More info can be found",
            tooltip(
              span(
                "here.",
                style = "text-decoration: underline; cursor: help;"
              ),
              p(strong("Referrals:"), "one record per period, with months_waited_id equal to 0."),
              p(strong("Incomplete:"),  "a record for each compartment for each period."),
              p(strong("Complete:"), "a record for each compartment for each period."),
              p("Note, only incompletes are used for the first period to provide the starting waiting list.")
            )
          ),
          hr(),
          layout_columns(
            col_widths = 12,
            downloadButton(
              outputId = ns("download_template"),
              label = "Download selections above as template"
            ),
            downloadLink(
              outputId = ns("sample_file"),
              label = "Download an example CSV file",
              class = "small-hyperlink"
            )
          ),
          hr(),
          fileInput(
            inputId = ns("fileInput"),
            label = "Upload your CSV file",
            accept = c("text/csv", ".csv"),
            placeholder = "Only CSV files are accepted"
          ),
          textOutput(ns("validation_message"))
        )
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
}

#' 02_planner Server Functions
#'
#' @importFrom shiny observeEvent renderUI dateInput tagList numericInput
#'   eventReactive Progress sliderInput HTML
#' @importFrom NHSRtt get_rtt_data latest_rtt_date convert_months_waited_to_id
#'   apply_params_to_projections apply_parameter_skew optimise_capacity
#' @importFrom lubridate `%m+%` `%m-%` floor_date ceiling_date interval
#' @importFrom dplyr mutate summarise arrange row_number cross_join left_join
#'   join_by bind_rows setdiff inner_join
#' @importFrom tidyr complete unnest
#' @importFrom purrr map2 map
#' @importFrom bslib tooltip value_box
#' @importFrom rlang .data
#' @noRd
mod_02_planner_server <- function(id, r){
  moduleServer( id, function(input, output, session){

# initial set up ----------------------------------------------------------

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
          spec = input$specialty_codes
        )

        # pass some values to the charting module
        r$chart_specification$trust <- selections_labels$trusts$display
        r$chart_specification$specialty <- selections_labels$specialties$display
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
            adjusted_referrals = .data$unadjusted_referrals +
              (.data$unadjusted_referrals * reactive_values$referrals_uplift),
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


# bring your own data -----------------------------------------------------


# sample data -------------------------------------------------------------

    # Provide sample CSV file for download
    output$sample_file <- downloadHandler(
      filename = function() {
        "sample_data.csv"
      },
      content = function(file) {
        # sample_data is an internal data object
        final_month <- NHSRtt::latest_rtt_date()
        sample_data_mnths <- unique(sample_data[["period"]]) |>
          sort()
        months_in_sample_data <- length(sample_data_mnths)
        calculated_first_month <- final_month %m-% months(months_in_sample_data - 1)

        mnth_lkp <- dplyr::tibble(
          period = sample_data_mnths,
          period_new = seq(
            from = calculated_first_month,
            to = final_month,
            by = "months"
          )
        )

        sample_data |>
          left_join(
            mnth_lkp,
            by = join_by(
              period
            )
          ) |>
          dplyr::select(
            period = "period_new",
            "months_waited_id",
            "type",
            "value"
          ) |>
          utils::write.csv(
            file,
            row.names = FALSE
          )
      }
    )


# template data -----------------------------------------------------------

    output$download_template <- downloadHandler(
      filename = function() {
        "template_data.csv"
      },
      content = function(file) {
        # sample_data is an internal data object
        max_download_date <- NHSRtt::latest_rtt_date()
        min_download_date <- lubridate::floor_date(
          max_download_date,
          unit = "months"
        ) %m-% months(input$calibration_months)

        # create progress bar
        progress <- Progress$new(
          session,
          min = 1,
          max = input$calibration_months + 1
        )

        on.exit(progress$close())
        progress$set(message = 'Downloading public data from RTT statistics',
                     detail = 'This will be included in template csv file')

        selections_labels <- filters_displays(
          trust_parents = input$trust_parent_codes,
          trusts = input$trust_codes,
          comm_parents = input$commissioner_parent_codes,
          comms = input$commissioner_org_codes,
          spec = input$specialty_codes
        )

        # download and aggregate data
        template_data <- get_rtt_data_with_progress(
          date_start = min_download_date,
          date_end = max_download_date,
          trust_parent_codes = selections_labels$trust_parents$selected_code,
          trust_codes = selections_labels$trusts$selected_code,
          commissioner_parent_codes = selections_labels$commissioner_parents$selected_code,
          commissioner_org_codes = selections_labels$commissioners$selected_code,
          specialty_codes = selections_labels$specialties$selected_code,
          progress = progress
        ) |>
          mutate(
            months_waited_id = NHSRtt::convert_months_waited_to_id(
              .data$months_waited,
              12 # this pools the data at 12+ months (this can be a user input in the future)
            )
          ) |>
          summarise(
            value = sum(.data$value),
            .by = c(
              "period", "months_waited_id", "type"
            )
          ) |>
          arrange(
            .data$type,
            .data$period,
            .data$months_waited_id
          )

        utils::write.csv(template_data, file, row.names = FALSE)
      }
    )

# uploaded data checks ----------------------------------------------------

    # Validate and read the uploaded file
    observeEvent(input$fileInput, {
      req(input$fileInput)

      # Read the file

      imported_data <- utils::read.csv(
        input$fileInput$datapath
      ) |>
        mutate(
          period = as.Date(.data$period)
        )

      # expected fields are "period", "type", "value", "months_waited_id" but
      # lots of other checks performed
      check_data <- check_imported_data(imported_data)

      import_msg <- output$validation_message <- renderText({
        check_data$msg
      })

      if (!is.null(check_data$imported_data_checked)) {
        imported_data <- check_data$imported_data_checked
      }
# browser()
      # create period lookup, but append the imported data to the start of the
      # horizon period so the start point of the projections begin at the end of
      # the imported period
      r$period_lkp <- imported_data |>
        # filter(.data$type == "Complete") |>
        distinct(.data$period) |>
        arrange(.data$period) |>
        bind_rows(
          dplyr::tibble(
            period = seq(
              from = forecast_dates()$start,
              to = forecast_dates()$end,
              by = "months"
            )
          )
        ) |>
        mutate(
          period_id = dplyr::row_number() - 1 # minus 1 because the first month in the imported data is the t0 incompletes
        )

        selections_labels <- filters_displays(
          trust_parents = input$trust_parent_codes,
          trusts = input$trust_codes,
          comm_parents = input$commissioner_parent_codes,
          comms = input$commissioner_org_codes,
          spec = input$specialty_codes
        )

        # pass some values to the charting module
        r$chart_specification$trust <- selections_labels$trusts$display
        r$chart_specification$specialty <- selections_labels$specialties$display
        r$chart_specification$observed_start <- min(imported_data[["period"]])
        r$chart_specification$observed_end <- max(imported_data[["period"]])

        r$all_data <- imported_data |>
          mutate(
            trust = selections_labels$trusts$display,
            specialty = selections_labels$specialties$display
          ) |>
          arrange(
            .data$trust,
            .data$specialty,
            .data$type,
            .data$months_waited_id,
            .data$period
          ) |>
          left_join(
            r$period_lkp,
            by = join_by(
              period
            )
          )

        reactive_values$data_downloaded <- TRUE

        # calculate "unadjusted" referrals (though referrals aren't being
        # adjusted here but the value is being passed through to the 3rd module
        # for transparency)
        unadjusted_referrals <- r$all_data |>
          filter(
            .data$type == "Referrals"
          ) |>
          dplyr::select(
            "period_id",
            "months_waited_id",
            unadjusted_referrals = "value"
          )

        # there is no uplift to referrals when bringing own data
        reactive_values$referrals_uplift <- 0

        # calculate the modelling parameters assuming referrals don't need to be
        # uplifted
        reactive_values$params <- calibrate_parameters(
          r$all_data,
          max_months_waited = 12,
          referrals_uplift = NULL,
          redistribute_m0_reneges = FALSE
        )

        # data frame of counts by period which get supplied to the 3rd module
        # for charting
        reactive_values$calibration_data <- calibrate_parameters(
          r$all_data,
          max_months_waited = 12,
          full_breakdown = TRUE,
          referrals_uplift = NULL,
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
            # we assume the referral inputs are the correct number if they aren't using the public data
            adjusted_referrals = .data$unadjusted_referrals +
              (.data$unadjusted_referrals * reactive_values$referrals_uplift),
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

    })


# calculate and populate forecast dates -----------------------------------

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

# calculate possible target dates from forecast horizone dates ----------------


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


# create the dynamic ui for target achievement date -----------------------

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


# calculate the latest performance ui from the data -----------------------

    # the latest performance value to be displayed
    output$latest_performance_ui <- shiny::renderUI({
      if (is.null(reactive_values$latest_performance)) {
        return(NULL)
      } else {
        # div(
          # p(reactive_values$latest_performance)
        value_box(
          title = "Latest performance",
          value = reactive_values$latest_performance,
          showcase = shiny::icon("chart-line")
        )
        # )
      }
    })


# create ui for multiple performance targets ------------------------------

    # create ui based on whether single or multiple target option selected
    # Initialize empty data frame

    target_data <- reactiveVal(
      dplyr::tibble(
      "Target_date" = as.Date(
        paste(lubridate::year(Sys.Date()) + 1, "03-01", sep = "-")
      ),
      "Target_percentage" = NA_real_
      )
    )

    # Add initial empty row
    observe({
      if (nrow(target_data()) == 0) {
        add_target()
      }
    }, priority = 1000)

    # Function to add a new target
    add_target <- function() {
      current_data <- target_data()
      new_row <- dplyr::tibble(
        "Target_date" = NA,
        "Target_percentage" = NA_real_
      )
      target_data(rbind(current_data, new_row))
    }

    # Add target button
    observeEvent(input$add_target, {
      add_target()
    })

    # Remove selected target
    observeEvent(input$remove_target, {
      if (!is.null(input$target_table_rows_selected)) {
        current_data <- target_data()
        selected_rows <- input$target_table_rows_selected
        if (length(selected_rows) > 0) {
          target_data(current_data[-selected_rows, , drop = FALSE])
          # Add a row if table becomes empty
          if (nrow(target_data()) == 0) {
            add_target()
          }
        }
      }
    })

    # Render editable table
    output$target_table <- renderDT({
      DT::datatable(
        target_data(),
        class = "customDT",
        editable = list(
          target = "cell",
          disable = list(columns = c())
        ),
        selection = "single",
        caption = "Double-click cell to edit",
        options = list(
          ordering = FALSE,
          pageLength = 10,
          dom = 't', #show table only
          autoWidth = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE,
        colnames = c(
          "Target date" = "Target_date",
          "Target percentage" = "Target_percentage"
        )
      )
    })

    # Handle cell edits
    observeEvent(input$target_table_cell_edit, {
      info <- input$target_table_cell_edit
      row <- info$row
      col <- info$col + 1  # Column indices start at 0 in JavaScript
      value <- info$value

      current_data <- target_data()

      # Validation for Target date
      if (colnames(current_data)[col] == "Target_date") {
        tryCatch({
          date_value <- as.Date(value)
          if (is.na(date_value)) {
            showNotification("Please enter a valid date (YYYY-MM-DD)", type = "error")
            return()
          }
          date_value <- lubridate::floor_date(
            date_value,
            unit = "months"
          )
          if (!dplyr::between(date_value, target_dates()$min, target_dates()$max)) {
            showNotification("Selected date must be at least one month after the start of the planning horizon", type = "error")
            return()
          } else if (date_value %in% current_data[["Target_date"]]) {
            showNotification("Only one date per month allowed", type = "error")
            return()
          }
          current_data[row, col] <- date_value
        }, error = function(e) {
          showNotification("Unknown error", type = "error")
          return()
        })
      }

      # Validation for Target percentage
      if (colnames(current_data)[col] == "Target_percentage") {
        tryCatch({
          pct_value <- as.numeric(value)
          if (is.na(pct_value) || pct_value < 0 || pct_value > 100) {
            showNotification("Percentage must be between 0 and 100", type = "error")
            return()
          }
          current_data[row, col] <- pct_value
        }, error = function(e) {
          showNotification("Percentage must be between 0 and 100", type = "error")
          return()
        })
      }

      target_data(current_data)
    })

# dynamic ui based on single or multiple targets --------------------------

    observeEvent(
      c(input$target_type), {
        if (input$target_type == "Single target") {
          output$target_type_input_ui <- shiny::renderUI({
            tagList(
              uiOutput(
                ns("target_achievement_date")
              ),
              uiOutput(
                ns("latest_performance")
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
              )
            )

          })
        } else if (input$target_type == "Multiple targets") {
          output$target_type_input_ui <- shiny::renderUI({
            card(
              card_header(
                class = "bg-dark",
                "Enter 4 month performance targets into table"
              ),
              card_body(
                height = '300px',
                fillable = TRUE,
                layout_sidebar(
                  sidebar = sidebar(
                    actionButton(
                      inputId = ns("add_target"),
                      label = "Add target",
                      class = "btn-primary"
                    ),
                    actionButton(
                      inputId = ns("remove_target"),
                      label = "Remove selected target",
                      class = "btn-danger"
                    )
                  ),
                  DTOutput(
                    ns("target_table")
                  )
                )
              ),
              width = "50%"
            )
          })
        }
      }
    )

# make scenario buttons appear if the data has already been downloaded --------

    output$optimise_capacity_ui <- renderUI({
      if (isTRUE(reactive_values$data_downloaded)) {
        bslib::input_task_button(
          id = ns("optimise_capacity"),
          label = "Run capacity optimisation",
          label_busy = "Forecasting...",
          type = "dark"
        )
      }
    })

    output$calculate_performance_ui <- renderUI({
      if (isTRUE(reactive_values$data_downloaded)) {
        bslib::input_task_button(
          id = ns("calculate_performance"),
          label = "Calculate future performance",
          label_busy = "Forecasting...",
          type = "dark"
        )
      }
    })


# dynamic ui card based on result of optimisation -------------------------

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


# dynamic ui for advanced skew manipulation -------------------------------

    # Advanced skew manipulation options
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

# dynamic UI based on the scenario choice ---------------------------------

      # Generate the dynamic UI based on dropdown selection
      if (input$interface_choice == "select") {
        tagList()
      } else if (input$interface_choice == "performance_inputs") {

        # Numeric interface
        tagList(
          layout_columns(
            col_widths = c(3, 4),
            span(
              "Select target type:",
            ),
            radioButtons(
              inputId = ns("target_type"),
              label = NULL,
              choices = c("Single target", "Multiple targets"),
              selected = "Single target"
            ),
            fill = FALSE
          ),
          uiOutput(
            ns("latest_performance_ui")
          ),
          uiOutput(
            ns("target_type_input_ui")
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
              selected = "Linear"
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
          layout_columns(
            col_widths = c(3, 4),
            span(
              "Align capacity and referrals after performance is achieved",
              tooltip(
                shiny::icon("info-circle"),
                shiny::HTML(
                  paste0(
                    "If capacity and referrals are changing at different rates, extreme future scenarios can occur, ",
                    "for example, waiting lists can clear if capacity is growing faster than referrals.<br><br>",
                    "This setting adjusts the capacity change to 'track' referrals once the performance target has been achieved.<br><br>",
                    "This has a stabilising impact on forecasts beyond the performance target date."
                  )
                ),
                placement = "right"
              )
            ),
            checkboxInput(
              inputId = ns("capacity_track_referrals"),
              label = NULL,
              value = TRUE
            ),
            fill = FALSE
          ),
          bslib::accordion(
            open = FALSE,
            id = "skew",
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
            id = "skew",
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

        if (input$calculate_performance >= 1) {

          forecast_months <- lubridate::interval(
            as.Date(input$forecast_date[[1]]),
            as.Date(input$forecast_date[[2]])
          ) %/% months(1) + 1

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
              adjusted_referrals = .data$unadjusted_referrals +
                (.data$unadjusted_referrals * reactive_values$referrals_uplift),
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
          r$chart_specification$target_data <- dplyr::tibble(
            Target_date = as.Date(character()),
            Target_performance = as.numeric()
          )
          r$chart_specification$optimise_status <- NULL
        }
      },
      ignoreInit = TRUE
    )


# optimising capacity based on performance inputs -------------------------

    observeEvent(
      c(input$optimise_capacity), {
        r$chart_specification$optimise_status <- NULL

        if (input$optimise_capacity >= 1) {

          skew <- dplyr::tibble(
            skew_param = seq(
              from = min(input$capacity_skew_range),
              to = max(input$capacity_skew_range),
              by = 0.05
            )
          )

          if (input$target_type == "Single target") {
            # replace the target_data reactiveVal with the single target inputs
            target_data(
              dplyr::tibble(
                "Target_date" = input$target_achievement_date,
                "Target_percentage" = input$target_value
              )
            )
          }

          # checks on target_data?

          unadjusted_baseline_referrals <- r$all_data |>
            filter(
              .data$type == "Referrals"
            )

          if (!is.null(reactive_values$referrals_uplift)) {
            baseline_referrals <- unadjusted_baseline_referrals |>
              mutate(
                value = .data$value +
                  (.data$value * reactive_values$referrals_uplift)
              )
          } else {
            baseline_referrals <- unadjusted_baseline_referrals
          }


          # referrals for planning horizon (based on uplifted numbers)

          forecast_months <- lubridate::interval(
            as.Date(input$forecast_date[[1]]),
            as.Date(input$forecast_date[[2]])
          ) %/% months(1) + 1 # the plus 1 makes is inclusive of the final month

          projections_referrals <- baseline_referrals |>
            filter(
              # first period only used for the count of incompletes
              .data$period != min(.data$period)
            ) |>
            forecast_function(
              number_timesteps = forecast_months,
              method = input$referral_growth_type,
              percent_change = input$referral_growth
            )

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
            )

          # start list to store the future capacity values
          projections_capacity_to_target <- list()
          projections_capacity_to_target[[1]] <- projections_capacity |>
            pull(.data$value)

          t1_capacity <- projections_capacity |>
            calculate_t1_value()

          baseline_incompletes <- r$all_data |>
            filter(
              .data$type == "Incomplete",
              .data$period == max(.data$period)
            ) |>
            select(
              "months_waited_id",
              incompletes = "value"
            )

          # note, baseline incompletes is used again when creating the final
          # dataset at the end
          t0_incompletes <- baseline_incompletes

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

          # create empty list for optimal skew parameters



          if (input$optimised_capacity_growth_type == "Uniform") {
            cap_prof <- "flat"
          } else if (input$optimised_capacity_growth_type == "Linear") {
            cap_prof <- "linear_change"
          }

          progress <- Progress$new(
            session,
            min = 1,
            max = nrow(skewed_params) * nrow(target_data()))
          on.exit(progress$close())

          progress$set(
            message = 'Calculating capacity change based on range of skews provided',
            detail = 'This may take a while...'
          )

          interval_start_date <- input$forecast_date[[1]]
          forecast_dates <- seq(
            from = input$forecast_date[[1]],
            to = input$forecast_date[[2]],
            by = "months"
          )

          # set up table to add the projections onto
          projection_calcs <- skewed_params |>
            mutate(
              # rowid is used for updating the progress bar
              rowid = dplyr::row_number(),
              capacity_projections = list(projections_capacity_to_target),
              start_capacity_1 = t1_capacity
            ) |>
            dplyr::cross_join(
              t0_incompletes
            ) |>
            tidyr::nest(
              incompletes_1 = c("months_waited_id", "incompletes")
            )

          for (i in seq_len(nrow(target_data()))) {

            i_target_data <- target_data() |>
              dplyr::slice(i)

            # create dummy value to store capacity projections to
            j <- i + 1

            start_date_id <- match(
              interval_start_date,
              forecast_dates
            )

            end_date_id <- match(
              i_target_data[["Target_date"]],
              forecast_dates
            )

            forecast_months_to_target <- lubridate::interval(
              as.Date(interval_start_date),
              as.Date(i_target_data[["Target_date"]])
            ) %/% months(1) + 1 # the plus 1 makes is inclusive of the final month

            # subset referrals for this interval
            interval_projected_referrals <- projections_referrals[start_date_id:end_date_id]

            # set new column names
            uplift_col <- paste("uplift", i, sep = "_")
            status_col <- paste("status", i, sep = "_")
            start_capacity_col <- paste("start_capacity", i, sep = "_")
            incompletes_col <- paste("incompletes", i, sep = "_")

            # col names for the next time period
            start_capacity_tj_col <- paste("start_capacity", j, sep = "_")
            incompletes_tj_col <- paste("incompletes", j, sep = "_")

            # calculate optimised uplift
            projection_calcs <- projection_calcs |>
              mutate(
                !!uplift_col := purrr::pmap(
                  .l = list(
                    par = .data[["params"]],
                    cap = .data[[start_capacity_col]],
                    incomp = .data[[incompletes_col]],
                    prog = .data[["rowid"]]
                  ),
                  .f = \(par, cap, incomp, prog) {

                    prog_val <- prog + ((i - 1) * nrow(projection_calcs))

                    progress$set(value = prog_val)
                    optimise_capacity(
                      t_1_capacity = cap,
                      referrals_projections = interval_projected_referrals,
                      incomplete_pathways = incomp,
                      renege_capacity_params = par,
                      target = paste0(100 - i_target_data[["Target_percentage"]], "%"),
                      target_bin = 4,
                      capacity_profile = cap_prof,
                      tolerance = 0.001,
                      max_iterations = 35
                    )
                  }
                ),
                !!status_col := names(unlist(.data[[uplift_col]])),
                !!uplift_col := as.numeric(.data[[uplift_col]]),
                capacity_projections = purrr::map2(
                  .x = .data[["capacity_projections"]],
                  .y = .data[[uplift_col]],
                  \(x, y) {
                    cap_projections <- dplyr::tibble(
                      value = x[[j - 1]]
                    ) |>
                      dplyr::mutate(
                        period_id = dplyr::row_number()
                      ) |>
                      forecast_function(
                        number_timesteps = forecast_months_to_target,
                        method = input$optimised_capacity_growth_type,
                        percent_change = (y - 1) * 100 # convert the uplift value into a percent
                      )

                    x[[j]] <- cap_projections
                    x
                  }
                ),
                # add start capacity for next time period
                !!start_capacity_tj_col := purrr::map_dbl(
                  .data$capacity_projections,
                  \(x) unlist(x) |>
                    tail(1)
                ),
                # add incompletes for the start of the next time period
                !!incompletes_tj_col := purrr::pmap(
                  .l = list(
                    cap_proj = .data[["capacity_projections"]],
                    incomp = .data[[incompletes_col]],
                    par = .data[["params"]]
                  ),
                  .f = \(cap_proj, incomp, par) {

                    apply_params_to_projections(
                      capacity_projections = cap_proj[[j]],
                      referrals_projections = interval_projected_referrals,
                      incomplete_pathways = incomp,
                      renege_capacity_params = par,
                      max_months_waited = 12
                    ) |>
                      filter(.data$period_id == max(.data$period_id)) |>
                      select(
                        "months_waited_id",
                        "incompletes"
                      )
                  }
                )

              )

            # start date for next target period
            interval_start_date <- i_target_data[["Target_date"]] %m+% months(1)

          }

          # are there remaining periods between the final target and the end of
          # the forecast period?

          if (interval_start_date <= utils::tail(forecast_dates, 1)) {
            forecast_months_to_end <- lubridate::interval(
              as.Date(interval_start_date),
              utils::tail(forecast_dates, 1)
            ) %/% months(1) + 1 # the plus 1 makes is inclusive of the final month

            if (isTRUE(input$capacity_track_referrals)) {
              # here we track referrals with capacity following target achievement


              # calculate the change in referrals for each period
              referrals_change_by_period <- unique(
                round(
                  diff(projections_referrals),
                  8
                )
              )

              # calculate post-target capacity
              projection_calcs <- projection_calcs |>
                mutate(
                  capacity_projections = purrr::map(
                    .data[["capacity_projections"]],
                    \(x) {
                      projections <- unlist(x) |>
                        tail(1) |>
                        (\(y) y + (seq_len(forecast_months_to_end) * referrals_change_by_period))()
                      x[[j + 1]] <- projections
                      x
                    }
                  )
                )

            } else {

              # continue projecting the last capacity change required to meet
              # the target onwards
              projection_calcs <- projection_calcs |>
                mutate(
                  capacity_projections = purrr::map2(
                    .x = .data[["capacity_projections"]],
                    .y = .data[[uplift_col]],
                    \(x, y) {

                      projections <- dplyr::tibble(
                        value = unlist(tail(x, 1))
                      ) |>
                        mutate(
                          period_id = dplyr::row_number()
                        ) |>
                        forecast_function(
                          number_timesteps = forecast_months_to_end,
                          method = input$optimised_capacity_growth_type,
                          percent_change = (y - 1) * 100 # convert the uplift value into a percent
                        )
                      x[[j + 1]] <- projections
                      x
                    }
                  )
                )
            }
          }

          projection_calcs <- projection_calcs |>
            mutate(
              capacity_projections = purrr::map(
                .data[["capacity_projections"]],
                \(x) {
                  x[[1]] <- NULL
                  x <- unlist(x) |>
                    # make negative capacity = 0
                    (\(x) ifelse(x < 0, 0, x))()
                }
              ),
              total_capacity = purrr::map_dbl(
                .data[["capacity_projections"]],
                sum
              )
            )

          # filter for the optimal scenario according to the selection

          projection_calcs <- projection_calcs |>
            dplyr::filter(
              .data$total_capacity == min(.data$total_capacity)
            )

          # filter(
          #   .data$uplift == min(.data$uplift)
          # ) |>
          # filter(
          #   # if there are multiple records that have the same capacity
          #   # uplift, select the record that has the smallest change from the
          #   # calibrated period's capacity utilisation profile (eg, the one
          #   # closest to 1)
          #   abs(.data$skew_param - 1) == min(abs(.data$skew_param - 1))
          # )

          # store the convergence status
          r$chart_specification$optimise_status <- projection_calcs |>
            dplyr::select(
              dplyr::starts_with("status")
            ) |>
            unlist() |>
            unname()

          # create capacity projections profile
          projections_capacity <- projection_calcs |>
            dplyr::pull(.data$capacity_projections) |>
            unlist()

          # calculate the unadjusted referrals for the projection period to
          # provide with the data to the charting section
          unadjusted_projections_referrals <- unadjusted_baseline_referrals |>
            filter(
              # first period only used for the count of incompletes
              .data$period != min(.data$period)
            ) |>
            forecast_function(
              number_timesteps = forecast_months,
              method = input$referral_growth_type,
              percent_change = input$referral_growth
            )

          r$waiting_list <- NHSRtt::apply_params_to_projections(
            capacity_projections = projections_capacity,
            referrals_projections = projections_referrals,
            incomplete_pathways = baseline_incompletes,
            renege_capacity_params = projection_calcs$params[[1]],
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
              adjusted_referrals = .data$unadjusted_referrals +
                (.data$unadjusted_referrals * reactive_values$referrals_uplift),
              period_id = .data$period_id + max(r$all_data$period_id),
              capacity_skew = projection_calcs$skew_param,
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
          # r$chart_specification$capacity_percent_change <- paste0(
          #   format(
          #     (projection_calcs$uplift - 1) * 100,
          #     format = "f",
          #     digits = 2,
          #     nsmall = 1
          #   ),
          #   "%"
          # )
          r$chart_specification$capacity_percent_change <- "NEEDS TO BE REVIEWED FOR MULTIPLE CHANGES IN CAPACITY"

          r$chart_specification$capacity_change_type <- input$optimised_capacity_growth_type
          r$chart_specification$capacity_skew <- projection_calcs$skew_param[[1]]
          r$chart_specification$target_data <- target_data()

          if (any(r$chart_specification$optimise_status == "waitlist_cleared")) {
            r$chart_specification$optimise_status <- "waitlist_cleared"
          } else if (all(r$chart_specification$optimise_status == "converged")) {
            r$chart_specification$optimise_status <- "converged"
          } else {
            r$chart_specification$optimise_status <- "unknown"
          }


          reactive_values$optimise_status_card_visible <- TRUE

        }
      }
    )

  })
}



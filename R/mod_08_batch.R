#' 08_batch UI Function
#'
#' @description Module that facilitates modelling the steady state waiting list
#'   size and distribution to achieve a stable future target
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS uiOutput numericInput selectizeInput helpText dateInput
#'   sliderInput hr
#' @importFrom bslib input_task_button card card_header layout_sidebar sidebar
#'   layout_columns card_body page_fillable bs_theme
mod_08_batch_ui <- function(id){
  ns <- NS(id)

  filters_sidebar <- sidebar(
    open = TRUE,
    width = '25%',
    selectizeInput(
      inputId = ns("ss_trust_codes"),
      label = "Select Trust(s)",
      choices = sort(unique(org_lkp$`Provider Org Name`)),
      options = list(
        placeholder = "Select one or more"
      ),
      multiple = TRUE
    ),
    checkboxInput(
      inputId = ns("ss_nhs_only"),
      label = "Show NHS providers only",
      value = TRUE,
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
    layout_columns(
      col_widths = c(8, 4),
      helpText("Low Referral Scenario"),
      numericInput(
        inputId = ns("referral_bin_low"),
        label = NULL,
        value = -1
      ),
      helpText("Medium Referral Scenario"),
      numericInput(
        inputId = ns("referral_bin_medium"),
        label = NULL,
        value = 0
      ),
      helpText("High Referral Scenario"),
      numericInput(
        inputId = ns("referral_bin_high"),
        label = NULL,
        value = 1
      )
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
      inputId = ns("target_value"),
      label = "Target proportion",
      min = 0,
      max = 100,
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
      )#,
      # uiOutput(ns("tick_mark_dwnld"))
    )
  )


  # Right Pane
  scenario_card <- card(
    card_header("Batch Output View"),
    card_body(
      # helpText("Coming Soon...")
      uiOutput(
        ns("ss_results_ui")
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
} # End UI


#' 08_batch Server Functions
#'
#' @importFrom shiny reactiveValues observeEvent renderUI helpText
#' @importFrom NHSRtt latest_rtt_date get_rtt_data
#' @importFrom lubridate floor_date interval
#' @importFrom rlang .data
#' @importFrom purrr map pmap list_rbind map2 map_dbl
#' @importFrom tidyr unnest nest complete
#' @importFrom dplyr filter mutate case_when select cross_join tibble row_number
#'   summarise rename left_join join_by intersect
#' @importFrom DT datatable renderDT DTOutput
#' @noRd
mod_08_batch_server <- function(id){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    reactive_values <- reactiveValues()
    reactive_values$show_results <- FALSE
    reactive_values$optimised_projections <- NULL

    # trust selection filtering based on other NHS only checkbox ----------------------
    reactive_org_tbl <- reactiveVal(org_lkp)

    observeEvent(
      c(input$ss_nhs_only), {

        reactive_org_tbl <- org_lkp

        if (isTRUE(input$ss_nhs_only)) {
          reactive_org_tbl <- reactive_org_tbl |>
            dplyr::filter(
              grepl("NHS", .data$`Provider Org Name`)
            )
        }

                # provider current selections
        current_provider <- dplyr::intersect(
          input$ss_trust_codes,
          unique(reactive_org_tbl[["Provider Org Name"]])
        )

        updateSelectizeInput(
          session,
          inputId = "ss_trust_codes",
          choices = sort(unique(reactive_org_tbl[["Provider Org Name"]])),
          selected = current_provider
        )
      })

    # modelling when batch run selected
    observeEvent(

      c(input$batch_run_rtt_data), {

        if (input$batch_run_rtt_data > 0) {
          # browser()
          # dummy trust names instead of input$ss_trust_codes
          dummy_trust_names <- c(
            "UNIVERSITY HOSPITALS DORSET NHS FOUNDATION TRUST",
            "UNIVERSITY HOSPITALS BRISTOL AND WESTON NHS FOUNDATION TRUST"
          )

          # dummy specialty names instead of input$specialty_codes
          dummy_specialty_names <- c(
            "General Surgery",
            "Total"
          )


          # translate input text into codes
          selections_labels <- filters_displays(
            nhs_regions = NA,
            nhs_only = input$ss_nhs_only,
            trust_parents = NA,
            trusts = dummy_trust_names,
            comm_parents = NA,
            comms = NA,
            spec = dummy_specialty_names
          )

          # the latest month of data to use for calibrating the models
          max_download_date <- NHSRtt::latest_rtt_date()

          # min date is the 13th month prior to the latest month of data
          min_download_date <- lubridate::floor_date(
            max_download_date,
            unit = "months"
          ) %m-% months(12)

          # pass codes into download function. AIM TO MAKE THIS PART SIMPLY LOOK
          # UP THE DATA FROM A TABLE STORED ON THE SERVER. THE DATA ON THE SERVER
          # SHOULD BE STRUCTURED LIKE raw_data
          raw_data <- seq(
            from = lubridate::floor_date(
              min_download_date, unit = "months"
            ),
            to = lubridate::floor_date(
              max_download_date, unit = "months"
            ),
            by = "months"
          ) |>
            purrr::map(
              ~ NHSRtt::get_rtt_data(
                date_start = .x,
                date_end = .x,
                trust_parent_codes = NULL,
                trust_codes = selections_labels$trusts$selected_code,
                commissioner_parent_codes = NULL,
                commissioner_org_codes = NULL,
                specialty_codes = selections_labels$specialties$selected_code
              )
            ) |>
            purrr::list_rbind() |>
            aggregate_and_format_raw_data(
              selected_specialties = selections_labels$specialties$selected_name,
              min_date = min_download_date,
              max_date = max_download_date
            )

          # calculate the referrals uplift value per specialty/trust
          referrals_uplift <- calibrate_parameters(
            raw_data,
            max_months_waited = 12,
            redistribute_m0_reneges = FALSE,
            referrals_uplift = NULL
          ) |>
            tidyr::unnest("params") |>
            dplyr::filter(
              .data$months_waited_id == 0
            ) |>
            dplyr::mutate(
              referrals_uplift = case_when(
                .data$renege_param < 0 ~ abs(.data$renege_param),
                .default = 0
              )
            ) |>
            select("trust", "specialty", "referrals_uplift")

          # calculate the modelling parameters using the uplifted referrals
          params <- calibrate_parameters(
            raw_data,
            max_months_waited = 12,
            redistribute_m0_reneges = FALSE,
            referrals_uplift = referrals_uplift
          )

          # number of months for forecasting
          forecast_months <- lubridate::interval(
            lubridate::floor_date(
              max_download_date,
              unit = "months"
            ) %m+% months(1),
            as.Date(input$target_date)
          ) %/% months(1) + 1 # the plus 1 makes is inclusive of the final month

          # calculate referrals for the three future referral scenarios

          projection_referrals <- raw_data |>
            filter(
              type == "Referrals",
              .data$period != min(.data$period)
            ) |>
            select(!c("type", "months_waited_id")) |>
            # uplift referrals based on underreporting of referrals in published data
            left_join(
              referrals_uplift,
              by = join_by(
                trust, specialty
              )
            ) |>
            mutate(
              value = .data$value + (.data$value * .data$referrals_uplift)
            ) |>
            select(
              !c("referrals_uplift")
            ) |>
            tidyr::complete(
              specialty = selections_labels$specialties$selected_name,
              period_id = setdiff(1, unique(raw_data$period_id)), # removes the first period
              trust = selections_labels$trusts$selected_name,
              fill = list(value = 0)
            ) |>
            tidyr::nest(
              cal_period = c("period", "period_id", "value")
            ) |>
            dplyr::cross_join(
              tibble(
                referrals_scenario = c(
                  "Low_referrals",
                  "Medium_referrals",
                  "High_referrals"
                ),
                referral_change = c(
                  input$referral_bin_low,
                  input$referral_bin_medium,
                  input$referral_bin_high
                )
              )
            ) |>
            mutate(
              # value = furrr::future_map2(
              value = purrr::map2(
                .x = .data$cal_period,
                .y = .data$referral_change,
                ~ forecast_function( # this is in the functions.R script
                  rtt_table = .x,
                  number_timesteps = forecast_months,
                  method = "Linear",
                  percent_change = .y
                )
              )
            ) |>
            select(!c("cal_period", "referral_change")) |>
            tidyr::unnest("value") |>
            # adjust referrals that are less than 0 to 0
            mutate(
              value = case_when(
                .data$value < 0 ~ 0,
                .default = .data$value
              ),
              period_id = dplyr::row_number() +
                max(raw_data$period_id),
              .by = c("trust", "specialty", "referrals_scenario")
            ) |>
            tidyr::nest(
              ref_projections = c(
                "period_id", "value"
              )
            )

          # calculate the capacity for the first projected timestep

          projection_capacity <- raw_data |>
            filter(
              type == "Complete",
              .data$period != min(.data$period)
            ) |>
            summarise(
              value = sum(.data$value),
              .by = c(
                "trust",
                "specialty",
                "period_id",
                "period"
              )
            ) |>
            tidyr::nest(
              cal_period = c("period", "period_id", "value")
            ) |>
            mutate(
              t_1_capacity = purrr::map_dbl(
                .data$cal_period,
                calculate_t1_value
              )
            ) |>
            select(!c("cal_period"))

          # INCOMPLETES at t = 0

          # Need to use the latest observed waiting list as the starting point for the
          # projections

          # observed incompletes at the end of the calibration period
          incompletes_at_t0 <- raw_data |>
            filter(
              .data$type == "Incomplete",
              .data$period_id == max(.data$period_id)
            ) |>
            select(!c("period", "period_id", "type")) |>
            tidyr::complete(
              specialty = selections_labels$specialties$selected_name,
              months_waited_id = setdiff(1, unique(raw_data$months_waited_id)), # removes first period
              trust = selections_labels$trusts$selected_name,
              fill = list(value = 0)
            ) |>
            rename(
              incompletes = "value"
            ) |>
            tidyr::nest(
              incompletes_t0 = c("months_waited_id", "incompletes")
            )

          # COMBINING DATA

          all_projection_data <- projection_capacity |>
            left_join(
              projection_referrals,
              by = join_by(
                trust, specialty
              ),
              relationship = "one-to-many"
            ) |>
            left_join(
              incompletes_at_t0,
              by = join_by(
                trust, specialty
              ),
              relationship = "many-to-one"
            ) |>
            left_join(
              params,
              by = join_by(
                trust, specialty
              ),
              relationship = "many-to-one"
            )

          # create period lookup
          period_lkp <- dplyr::tibble(
            period_id = seq_len(max(raw_data$period_id) + forecast_months),
            period = seq(
              from = min(raw_data$period),
              to = input$target_date,
              by = "months"
            )
          )

          # ignore specialties-trust combinations where lower than threshold number of
          # treatments have occurred in calibration year
          threshold <- 50

          poor_calibration <- raw_data |>
            filter(
              .data$type == "Complete"
            ) |>
            summarise(
              value = sum(.data$value),
              .by = c(
                "trust", "specialty"
              )
            ) |>
            tidyr::complete(
              .data$specialty,
              .data$trust,
              fill = list(value = 0)
            ) |>
            filter(
              .data$value <= threshold
            ) |>
            select("trust", "specialty") |>
            mutate(status = "low_completed_pathways_in_calibration_period")

          # optimise capacity to achieve target
          reactive_values$optimised_projections <- all_projection_data |>
            left_join(
              poor_calibration,
              by = join_by(
                trust, specialty
              ),
              relationship = "many-to-one"
            ) |>
            mutate(
              annual_linear_uplift = case_when(
                # is.na(.data$status) = furrr::future_pmap(
                is.na(.data$status) ~ purrr::pmap(
                  .l = list(
                    .data$t_1_capacity,
                    .data$ref_projections,
                    .data$incompletes_t0,
                    .data$params
                  ),
                  .f = \(t_1_cap, ref_proj, incomp_t0, params) optimise_capacity(
                    t_1_capacity = t_1_cap,
                    referrals_projections = ref_proj |> pull(.data$value),
                    incomplete_pathways = incomp_t0,
                    renege_capacity_params = params,
                    target = paste0(100 - input$target_value, "%"),
                    target_bin = 4,
                    tolerance = 0.001,
                    max_iterations = 35
                  )
                ),
                .default = list(low_completed_pathways_in_calibration_period = Inf)
              ),
              status = names(unlist(.data$annual_linear_uplift)),
              annual_linear_uplift = as.numeric(.data$annual_linear_uplift)
            ) |>
            select(
              "trust",
              "specialty",
              "referrals_scenario",
              "t_1_capacity",
              "annual_linear_uplift",
              "status"
            )



          reactive_values$show_results <- TRUE
        }

      }
    )

    # create the result table
    output$results_table <- DT::renderDT({

      if (reactive_values$show_results ==  TRUE) {
        DT::datatable(
          reactive_values$optimised_projections
        )
      } else {
        NULL
      }
    })

    # dynamic display, only show if results have been created
    output$ss_results_ui <- renderUI({
      if (reactive_values$show_results ==  TRUE) {

        DTOutput(
          ns("results_table")
        )
      } else {
        helpText("Please make selections and generate results")
      }
    })
  })
}



## To be copied in the UI
# mod_08_batch_ui("08_batch_1")

## To be copied in the server
# mod_08_batch_server("08_batch_1")

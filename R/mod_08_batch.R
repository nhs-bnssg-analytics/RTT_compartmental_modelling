#' 08_batch UI Function
#'
#' @description Module that facilitates modelling the steady state waiting list
#'   size and distribution to achieve a stable future target
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS uiOutput numericInput selectizeInput p dateInput
#'   sliderInput hr radioButtons
#' @importFrom bslib input_task_button card card_header layout_sidebar sidebar
#'   layout_columns card_body page_fillable bs_theme
#' @importFrom lubridate years ceiling_date `%m+%`
#' @importFrom shinyWidgets pickerInput
mod_08_batch_ui <- function(id) {
  ns <- NS(id)

  filters_sidebar <- sidebar(
    open = TRUE,
    width = '25%',
    pickerInput(
      ns("selectedregions"),
      "Region(s):",
      choices = sort(unique(org_lkp_ss_inputs$Region)),
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count",
        `count-selected-text` = "{0} Selections (on a total of {1})"
      ),
      selected = sort(unique(org_lkp_ss_inputs$Trust)),
      multiple = TRUE
    ),
    radioButtons(
      inputId = ns("ss_nhs_only"),
      label = NULL,
      choiceNames = list(
        "NHS providers",
        span(
          "Non-NHS providers",
          tooltip(
            shiny::icon("info-circle"),
            "Includes community providers",
            placement = "right"
          )
        ),
        "All providers"
      ),
      choiceValues = list(
        "nhs_only",
        "non_nhs_only",
        "all"
      ),
      selected = "nhs_only",
      inline = TRUE
    ),
    selectizeInput(
      inputId = ns("specialty_codes"),
      label = "Select specialties",
      choices = unname(treatment_function_codes),
      options = list(
        placeholder = "Select one or more"
      ),
      multiple = TRUE # Enabled
    ),
    layout_columns(
      col_widths = c(8, 4),
      p("Low Referral Scenario (% annual uplift)"),
      numericInput(
        inputId = ns("referral_bin_low"),
        label = NULL,
        value = -1
      ),
      p("Medium Referral Scenario (% annual uplift)"),
      numericInput(
        inputId = ns("referral_bin_medium"),
        label = NULL,
        value = 0
      ),
      p("High Referral Scenario (% annual uplift)"),
      numericInput(
        inputId = ns("referral_bin_high"),
        label = NULL,
        value = 1
      )
    ),
    hr(),
    layout_columns(
      col_widths = c(8, 4),
      p(name_with_tooltip(
        "Target date",
        definition = "The date to achieve the target criteria by"
      )),
      dateInput(
        inputId = ns("target_date"),
        label = NULL,
        min = lubridate::ceiling_date(Sys.Date(), unit = "month"),
        max = lubridate::ceiling_date(
          Sys.Date() %m+% years(10),
          unit = "month"
        ),
        value = "2029-03-01",
        format = "dd-mm-yyyy",
        weekstart = 1,
        autoclose = TRUE #,
        # width = "40%"
      ),
      p(name_with_tooltip(
        "Target week",
        definition = "The week that the 'target proportion' applies to"
      )),
      numericInput(
        inputId = ns("target_week"),
        label = NULL,
        min = 1,
        max = 52,
        value = 18
      ),
      p(name_with_tooltip(
        "Target proportion",
        definition = "The proportion of people on a waiting list that have been waiting less than the 'target week'"
      )),
      sliderInput(
        inputId = ns("target_value"),
        label = NULL,
        min = 0,
        max = 100,
        value = 92,
        step = 1,
        post = "%",
        width = "100%"
      )
    ),
    hr(),
    layout_columns(
      col_widths = c(12),
      bslib::input_task_button(
        id = ns("batch_run_rtt_data"),
        label = "Calculate steady state",
        label_busy = "Running...",
        type = "dark"
      ),
      radioButtons(
        inputId = ns("ss_method"),
        label = span(
          "Chose solution method based on:",
          tooltip(
            shiny::icon("info-circle"),
            shiny::HTML(
              paste0(
                "Theoretically, there are an infinite number of solutions that can achieve steady state ",
                "(e.g., where referrals are equal to the sum of treatments and reneges) by varying the volume of treatment,",
                " and the profile of how the treatment is applied across the waiting list.<br>",
                "A single solution can be identified in two ways:<br><br>",
                "<strong>Treatments:</strong> ",
                "selected solution is as close to the current treatment capacity as possible.",
                "This can result in a larger waiting list size, which can have knock on effects of additional healthcare requirements.<br><br>",
                "<strong>Renege rates:</strong> ",
                "the proportion or people reneging out of the total people leaving the waiting list is similar to the national rate for that specialty",
                " when the 92% target was last met (or the best historic performance if this never happened)."
              )
            ),
            placement = "right"
          )
        ),
        choices = list(
          Treatments = "treatments",
          `Renege rates` = "renege_rates"
        ),
        selected = "renege_rates"
      )
    )
  )

  # Right Pane
  scenario_card <- card(
    card_header("Batch Output View"),
    card_body(
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          title = "Description of methods",
          p(
            HTML(paste(
              "The results here represent 'steady-state' scenarios.",
              "Here, 'steady-state' means that arrivals onto the waiting list are equal to departures, where arrivals are referrals, and departures are both treatments and reneges (see 'Definitions').",
              "A second criterion is also fulfilled from these results - that the calculated waiting list is achieving the desired performance target.",
              "<br>The steps to identify the resulting solutions are as follows:",
              "First, the final 12 months of available public data are used to understand, on average, the proportion of people that renege who are on the waiting list, by how long they have been waiting.",
              "Second, testing for a range of total treatments, a corresponding treatment profile that follows a <a href='https://en.wikipedia.org/wiki/Geometric_distribution' target='_blank'>geometric distribution</a> is identified that result in the two criteria described to be met - once reneging has also been accounted for based on the rates calculated from the calibration period.",
              "From this range of solutions, a single solution is selected based on the user defined solution method.",
              sep = "<br>"
            ))
          )
        )
      ),
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
#' @importFrom shiny reactiveValues observeEvent renderUI p modalDialog modalButton
#'   tagList showModal plotOutput renderPlot withTags
#' @importFrom NHSRtt latest_rtt_date get_rtt_data find_p
#' @importFrom lubridate floor_date interval
#' @importFrom rlang .data
#' @importFrom purrr map pmap list_rbind map2 map_dbl
#' @importFrom tidyr unnest nest complete
#' @importFrom dplyr filter mutate case_when select cross_join tibble row_number
#'   summarise rename left_join join_by intersect all_of distinct slice relocate
#'   across
#' @importFrom reactable reactable renderReactable colDef colFormat reactableOutput
#'   colGroup
#' @importFrom shinyWidgets updatePickerInput
#' @noRd
mod_08_batch_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    reactive_values <- reactiveValues()
    reactive_values$show_results <- FALSE # determines whether outputs are shown
    reactive_values$optimised_projections <- NULL # these are the outputs
    reactive_values$optimised_waiting_list <- NULL # these are the outputs

    # trust selection filtering based on other NHS only checkbox ----------------------
    reactive_org_tbl <- reactiveVal(org_lkp_ss_inputs) 

    # Inputs
    observeEvent(
      c(input$selectedregions, input$ss_nhs_only),
      {
        reactive_org_tbl <- org_lkp_ss_inputs
        
        if (input$ss_nhs_only == "nhs_only") {
          reactive_org_tbl <- reactive_org_tbl |>
            dplyr::filter(
              grepl("NHS", .data$Trust)
            )
        } else if (input$ss_nhs_only == "non_nhs_only") {
          reactive_org_tbl <- reactive_org_tbl |>
            dplyr::filter(
              !grepl("NHS", .data$Trust)
            )
        }

        choicesI <- reactive_org_tbl |>
          dplyr::filter(.data$Region %in% input$selectedregions) |>
          dplyr::distinct(.data$ICB) |>
          dplyr::arrange(.data$ICB)

        # trust current selections
        current_icbs <- dplyr::intersect(
          input$selectedICBs,
          unique(reactive_org_tbl[["ICB"]])
        )

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "selectedICBs",
          choices = choicesI$ICB,
          selected = current_icbs
        )
      },
      ignoreInit = FALSE
    )

    observeEvent(
      c(input$selectedICBs, input$ss_nhs_only),
      {
        reactive_org_tbl <- org_lkp_ss_inputs

        if (input$ss_nhs_only == "nhs_only") {
          reactive_org_tbl <- reactive_org_tbl |>
            dplyr::filter(
              grepl("NHS", .data$Trust)
            )
        } else if (input$ss_nhs_only == "non_nhs_only") {
          reactive_org_tbl <- reactive_org_tbl |>
            dplyr::filter(
              !grepl("NHS", .data$Trust)
            )
        }

        choicesT <- reactive_org_tbl |>
          filter(.data$ICB %in% input$selectedICBs) |>
          select(.data$Trust) |>
          unique() |>
          arrange(.data$Trust)

        # trust current selections
        current_provider <- dplyr::intersect(
          input$selectedtrusts,
          unique(reactive_org_tbl[["Trust"]])
        )

        updatePickerInput(
          session = session,
          inputId = "selectedtrusts",
          choices = choicesT$Trust,
          selected = current_provider
        )
      },
      ignoreInit = FALSE
    )

    # perform modelling when batch run selected -------------------------------

    observeEvent(
      c(input$batch_run_rtt_data),
      {
        if (input$batch_run_rtt_data > 0) {
          if (is.null(input$selectedtrusts) || is.null(input$specialty_codes)) {
            # If input is empty, show a modal dialog (popup)
            showModal(
              modalDialog(
                title = "Input Error",
                "Please make a selection for both Trust & Specialty before submitting",
                easyClose = TRUE, # Allows closing by clicking outside the modal
                footer = tagList(
                  modalButton("Close")
                )
              )
            )
          } else {
            # translate input values into codes for subsequent functions
            selections_labels <- filters_displays(
              nhs_regions = NA,
              nhs_only = input$ss_nhs_only,
              trust_parents = NA,
              trusts = input$selectedtrusts,
              comm_parents = NA,
              comms = NA,
              spec = input$specialty_codes
            )

            # pass codes into download function. AIM TO MAKE THIS PART SIMPLY LOOK
            # UP THE DATA FROM A TABLE STORED ON THE SERVER. THE DATA ON THE
            # SERVER SHOULD BE STRUCTURED LIKE raw_data FOLLOWING THIS CHUNK OF
            # CODE (action: EI). The data on the server here will also be used in
            # module 2.

            # RDS VERSION
            raw_data <- readRDS(system.file(
              "extdata",
              "rtt_12months.rds",
              package = "RTTshiny"
            )) |>
              clean_raw_data() |>
              filter(.data$trust %in% input$ss_trust_codes) |>
              filter(.data$specialty %in% c(input$specialty_codes))

            # the latest month of data to use for calibrating the models
            max_download_date <- max(raw_data$period)

            # calculate the number of months for projection period
            forecast_months <- lubridate::interval(
              lubridate::floor_date(
                max_download_date,
                unit = "months"
              ) %m+%
                months(1),
              as.Date(input$target_date)
            ) %/%
              months(1) +
              1 # the plus 1 makes is inclusive of the final month

            current <- append_current_status(
              data = raw_data,
              max_months_waited = 12,
              percentile = input$target_value / 100,
              percentile_month = convert_weeks_to_months(input$target_week)
            ) |>
              # add the referrals scenarios
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
                  ~ forecast_function(
                    # this is in the functions.R script
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
                  "period_id",
                  "value"
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

            # Here we use the latest observed waiting list as the starting point
            # for the projections
            incompletes_at_t0 <- raw_data |>
              filter(
                .data$type == "Incomplete",
                .data$period_id == max(.data$period_id)
              ) |>
              select(!c("period", "period_id", "type")) |>
              tidyr::complete(
                specialty = selections_labels$specialties$selected_name,
                months_waited_id = setdiff(
                  1,
                  unique(raw_data$months_waited_id)
                ), # removes first period
                trust = selections_labels$trusts$selected_name,
                fill = list(value = 0)
              ) |>
              rename(
                incompletes = "value"
              ) |>
              tidyr::nest(
                incompletes_t0 = c("months_waited_id", "incompletes")
              )

            # combine referral, t1 capacity, t0 incompletes, and params into one
            # dataset where each row is a different trust, specialty and referral
            # scenario
            all_projection_data <- projection_capacity |>
              left_join(
                projection_referrals,
                by = join_by(
                  trust,
                  specialty
                ),
                relationship = "one-to-many"
              ) |>
              left_join(
                incompletes_at_t0,
                by = join_by(
                  trust,
                  specialty
                ),
                relationship = "many-to-one"
              ) |>
              left_join(
                params,
                by = join_by(
                  trust,
                  specialty
                ),
                relationship = "many-to-one"
              )

            # create period to period_id lookup
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
                  "trust",
                  "specialty"
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

            # optimise capacity to achieve target - NOTE, THIS IS THE OTHER PART
            # WHERE THE RESULTS CAN BE STORED ON THE SERVER. PREFERABLY, THE USER
            # WOULD SELECT ALL OF THEIR PREFERRED INPUTS, AND THEN THE MODULE
            # WOULD LOOK TO A DATABASE OF RESULTS AND IF THERE ARE RESULTS FOR
            # THAT COMBINATION OF TRUST/SPECIALTY/REFERRAL SCENARIO/TARGET AND
            # TARGET DATE, IT WOULD PRESENT THE RESULTS IMMEDIATELY, OTHERWISE IT
            # WOULD CALCULATE THEM (action: EI)
            reactive_values$optimised_projections <- all_projection_data |>
              left_join(
                poor_calibration,
                by = join_by(
                  trust,
                  specialty
                ),
                relationship = "many-to-one"
              ) |>
              mutate(
                annual_linear_uplift = case_when(
                  # is.na(.data$status) = furrr::future_pmap(
                  is.na(.data$status) ~
                    purrr::pmap(
                      .l = list(
                        .data$t_1_capacity,
                        .data$ref_projections,
                        .data$incompletes_t0,
                        .data$params
                      ),
                      \(cap, ref_start, ref_end, inc, par, t, sp) {
                        # cat(paste(t, sp))
                        # cat("\n")
                        append_counterfactual(
                          capacity = cap,
                          referrals_start = ref_start,
                          referrals_end = ref_end,
                          incompletes_t0 = inc,
                          renege_capacity_params = par,
                          forecast_months = forecast_months,
                          target_week = input$target_week
                        )
                      }
                    )
                  ) |>
                  tidyr::unnest("counterfactual") |>
                  dplyr::select(
                    !c(
                      "params",
                      "referral_change",
                      "id",
                      "renege_proportion",
                      "target",
                      "wl_ss",
                      "wl_t0"
                    )
                  ) |>
                  mutate(
                    referrals_counterf = .data$referrals_ss,
                    capacity_counterf = .data$capacity_t1
                  ) |>
                  distinct() |>
                  dplyr::relocate(
                    dplyr::all_of(
                      c(
                        "referrals_counterf",
                        "capacity_counterf",
                        "reneges_counterf",
                        "incompletes_counterf",
                        "perf_counterf"
                      )
                    ),
                    .before = "referrals_ss"
                  )
              }
            )
            reactive_values$show_results <- TRUE
          }
        }
      }
    )

    output$plot_waiting_lists_ui <- renderUI({
      plot_waiting_lists <- lapply(
        seq_len(nrow(reactive_values$optimised_waiting_list)),
        function(i) {
          local({
            index <- i
            plotname <- paste0("plot_wl", index)

            output[[plotname]] <- renderPlot({
              reactive_values$optimised_waiting_list |>
                dplyr::slice(i) |>
                tidyr::pivot_longer(
                  cols = c(
                    "steady_state_waiting_list",
                    "previous_waiting_list"
                  ),
                  names_to = "wl_type",
                  values_to = "wl_data"
                ) |>
                tidyr::unnest("wl_data") |>
                mutate(
                  wl_description = factor(
                    .data$wl_description,
                    levels = c(
                      format(
                        max(.data$period, na.rm = TRUE) %m-% months(12),
                        format = "%b %Y"
                      ),
                      format(max(.data$period, na.rm = TRUE), format = "%b %Y"),
                      "Steady state"
                    )
                  )
                ) |>
                plot_waiting_lists_chart(
                  target_week = input$target_week,
                  target_value = input$target_value
                )
            })
          })
        }
      )
      NULL # UI already handled in reactable details
    })

    # create the result table
    output$results_table <- reactable::renderReactable({
      if (reactive_values$show_results == TRUE) {
        # browser()
        # create the grouping columns
        current_cols <- c(
          "referrals_t1",
          "capacity_t1",
          "reneges_t0",
          "load",
          "incompletes_t0",
          "pressure"
        )

        ss_cols <- c(
          "referrals_ss",
          "capacity_ss",
          "reneges_ss",
          "incompletes_ss"
        )

        counterf_cols <- c(
          "referrals_scenario",
          "referrals_counterf",
          "capacity_counterf",
          "reneges_counterf",
          "incompletes_counterf",
          "perf_counterf"
        )
        reactable::reactable(
          reactive_values$optimised_projections,
          columnGroups = list(
            colGroup(
              name = "Current",
              columns = current_cols
            ),
            colGroup(name = "Do nothing scenario", columns = counterf_cols),
            colGroup(name = "Steady state", columns = ss_cols)
          ),
          filterable = TRUE,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 20, 50, 100),
          height = 800, # this makes headers sticky and table scrollable
          defaultPageSize = 10,
          defaultColDef = colDef(
            vAlign = "center",
            headerVAlign = "bottom",
            headerClass = "header"
          ),
          columns = list(
            trust = colDef(
              header = name_with_tooltip("Trust", definition = "Trust name"),
              sticky = "left"
            ),
            specialty = colDef(
              header = name_with_tooltip(
                "Specialty",
                definition = "Specialty name"
              ),
              class = "divider-right",
              sticky = "left"
            ),
            referrals_t1 = colDef(
              header = name_with_tooltip(
                "Demand",
                definition = "The calculated current demand based on the previous 12 months"
              ),
              format = colFormat(digits = 2)
            ),
            capacity_t1 = colDef(
              header = name_with_tooltip(
                "Treatments",
                definition = "The calculated current treatment capacity based on the previous 12 months"
              ),
              format = colFormat(digits = 2)
            ),
            reneges_t0 = colDef(
              header = name_with_tooltip(
                "Reneges",
                definition = "Calculated total reneges for final month of 12 month calibration period"
              ),
              format = colFormat(digits = 2)
            ),
            load = colDef(
              header = name_with_tooltip(
                "Load",
                definition = "Arrivals (demand) divided by departures (treatments + reneges), indicating whether list size is growing (>1) or shrinking (<1)"
              ),
              format = colFormat(digits = 2),
              style = function(value) {
                list(
                  backgroundColor = cell_colour(
                    currentval = value,
                    lowval = c(
                      "#94e994ff" = min(
                        reactive_values$optimised_projections$load,
                        0.5,
                        na.rm = TRUE
                      )
                    ),
                    midval = c(
                      "#FCFAFA" = 1
                    ),
                    highval = c(
                      "#FA7557" = max(
                        reactive_values$optimised_projections$load,
                        2,
                        na.rm = TRUE
                      )
                    )
                  )
                )
              }
            ),
            incompletes_t0 = colDef(
              header = name_with_tooltip(
                "Waiting list size",
                definition = "The number of people on the waiting list for the last observed period"
              ),
              format = colFormat(digits = 0)
            ),
            pressure = colDef(
              header = name_with_tooltip(
                "Pressure",
                definition = "The current target percentile waiting time divided by the expected target percentilile wait time"
              ),
              format = colFormat(digits = 2),
              class = "divider-right",
              style = function(value) {
                list(
                  backgroundColor = cell_colour(
                    currentval = value,
                    lowval = c(
                      "#94e994ff" = min(
                        reactive_values$optimised_projections$pressure,
                        0.5,
                        na.rm = TRUE
                      )
                    ),
                    midval = c(
                      "#FCFAFA" = 1
                    ),
                    highval = c(
                      "#FA7557" = max(
                        reactive_values$optimised_projections$pressure,
                        2,
                        na.rm = TRUE
                      )
                    )
                  )
                )
              }
            ),
            referrals_scenario = colDef(
              header = name_with_tooltip(
                "Demand scenario",
                definition = "The demand scenario based on the user inputs"
              )
            ),
            referrals_counterf = colDef(
              header = name_with_tooltip(
                "Demand",
                definition = "The demand at the the target date based on the user inputs"
              ),
              format = colFormat(digits = 2)
            ),
            capacity_counterf = colDef(
              header = name_with_tooltip(
                "Treatments",
                definition = "The calculated number of monthly treatments under the 'do nothing' scenario at the target date"
              ),
              format = colFormat(digits = 2)
            ),
            reneges_counterf = colDef(
              header = name_with_tooltip(
                "Reneges",
                definition = "The calculated number of monthly reneges under the 'do nothing' scenario at the target date"
              ),
              format = colFormat(digits = 2)
            ),
            incompletes_counterf = colDef(
              header = name_with_tooltip(
                "Waiting list size",
                definition = "The number of people on the waiting list under the 'do nothing' scenario at the target date"
              ),
              format = colFormat(digits = 0)
            ),
            perf_counterf = colDef(
              header = name_with_tooltip(
                paste0(input$target_week, " week performance"),
                definition = "The proportion of people on the waiting list waiting less than the target time under the 'do nothing' scenario at the target date"
              ),
              format = colFormat(digits = 1, percent = TRUE),
              class = "divider-right",
              style = function(value) {
                list(
                  backgroundColor = cell_colour(
                    currentval = value,
                    lowval = c(
                      "#FA7557" = min(
                        reactive_values$optimised_projections$perf_counterf,
                        0,
                        na.rm = TRUE
                      )
                    ),
                    midval = c(
                      "#FCFAFA" = input$target_value
                    ),
                    highval = c(
                      "#94e994ff" = max(
                        reactive_values$optimised_projections$perf_counterf,
                        1,
                        na.rm = TRUE
                      )
                    )
                  )
                )
              }
            ),
            referrals_ss = colDef(
              header = name_with_tooltip(
                "Demand",
                definition = "The demand at the the target date based on the user inputs"
              ),
              format = colFormat(digits = 2)
            ),
            capacity_ss = colDef(
              header = name_with_tooltip(
                "Treatments",
                definition = "The calculated number of monthly treatments to achieve the steady-state solution based on the selected method"
              ),
              format = colFormat(digits = 2)
            ),
            reneges_ss = colDef(
              header = name_with_tooltip(
                "Reneges",
                definition = "The calculated number of monthly reneges in the steady-state solution based on the selected method"
              ),
              format = colFormat(digits = 2)
            ),
            incompletes_ss = colDef(
              header = name_with_tooltip(
                "Waiting list size",
                definition = "The number of people on the waiting list in the steady-state solution based on the selected method"
              ),
              format = colFormat(digits = 0),
              class = "divider-right"
            ),
            current_vs_ss_wl_ratio = colDef(
              header = name_with_tooltip(
                "Current / steady state waiting list size",
                definition = "The ratio of the current waiting list size compared with the calculated steady-state waiting list size"
              ),
              format = colFormat(digits = 2),
              style = function(value) {
                list(
                  backgroundColor = cell_colour(
                    currentval = value,
                    lowval = c(
                      "#94e994ff" = min(
                        reactive_values$optimised_projections$current_vs_ss_wl_ratio,
                        0.5,
                        na.rm = TRUE
                      )
                    ),
                    midval = c(
                      "#FCFAFA" = 1
                    ),
                    highval = c(
                      "#FA7557" = max(
                        reactive_values$optimised_projections$current_vs_ss_wl_ratio,
                        2,
                        na.rm = TRUE
                      )
                    )
                  )
                )
              }
            ),
            monthly_removals = colDef(
              header = name_with_tooltip(
                "Additional monthly removals required",
                definition = "The number of additional monthly removals (above current treatments and reneges) to achieve the target waiting list size in the number of months specified by the user"
              ),
              format = colFormat(digits = 2),
              style = function(value) {
                list(
                  backgroundColor = cell_colour(
                    currentval = value,
                    lowval = c(
                      "#94e994ff" = min(
                        reactive_values$optimised_projections$monthly_removals,
                        -1,
                        na.rm = TRUE
                      )
                    ),
                    midval = c(
                      "#FCFAFA" = 0
                    ),
                    highval = c(
                      "#FA7557" = max(
                        reactive_values$optimised_projections$monthly_removals,
                        2,
                        na.rm = TRUE
                      )
                    )
                  )
                )
              }
            )
          ),
          onClick = "expand",
          detail = function(index) {
            div(
              style = "padding: 16px;",
              plotOutput(
                outputId = ns(paste0("plot_wl", index)),
                height = "600px"
              )
            )
          },
          class = "steadystate-table"
        )
      } else {
        NULL
      }
    })

    observeEvent(
      c(input$copy_results),
      {
        if (input$copy_results > 0) {
          reactive_values$optimised_projections |>
            dplyr::rename(
              dplyr::all_of(
                c(
                  "Trust" = "trust",
                  "Specialty" = "specialty",
                  "Current demand" = "referrals_t1",
                  "Current treatment capacity" = "capacity_t1",
                  "Current reneges" = "reneges_t0",
                  "Current load" = "load",
                  "Current waiting list size" = "incompletes_t0",
                  "Current pressure" = "pressure",
                  "Demand scenario" = "referrals_scenario",
                  "Do nothing demand" = "referrals_counterf",
                  "Do nothing treatment capacity" = "capacity_counterf",
                  "Do nothing reneges" = "reneges_counterf",
                  "Do nothing waiting list size" = "incompletes_counterf",
                  "Do nothing performance" = "perf_counterf",
                  "Steady state demand" = "referrals_ss",
                  "Steady state treatment capacity" = "capacity_ss",
                  "Steady state reneges" = "reneges_ss",
                  "Steady state waiting list size" = "incompletes_ss",
                  "Current / steady state waiting list size" = "current_vs_ss_wl_ratio",
                  "Additional monthly removals required" = "monthly_removals"
                )
              )
            ) |>
            utils::write.table(
              file = "clipboard",
              sep = "\t",
              row.names = FALSE
            )
          showModal(modalDialog(
            title = "Copy success",
            "Results copied to clipboard",
            easyClose = TRUE,
            footer = NULL
          ))
        }
      }
    )

    observeEvent(
      c(input$copy_wl_results),
      {
        if (input$copy_wl_results > 0) {
          reactive_values$optimised_waiting_list |>
            dplyr::select(
              "trust",
              "specialty",
              "referrals_scenario",
              "steady_state_waiting_list"
            ) |>
            tidyr::unnest("steady_state_waiting_list") |>
            mutate(
              months_waited_id = case_when(
                .data$months_waited_id == max(.data$months_waited_id) ~
                  paste0(.data$months_waited_id + 1, "+"),
                .default = as.character(.data$months_waited_id + 1)
              )
            ) |>
            dplyr::select(
              c(
                "Trust" = "trust",
                "Specialty" = "specialty",
                "Demand scenario" = "referrals_scenario",
                "nth month of waiting" = "months_waited_id",
                "Steady state treatment capacity" = "sigma",
                "Steady state incomplete pathways" = "wlsize"
              )
            ) |>
            utils::write.table(
              file = "clipboard",
              sep = "\t",
              row.names = FALSE
            )
          showModal(modalDialog(
            title = "Copy success",
            "Results copied to clipboard",
            easyClose = TRUE,
            footer = NULL
          ))
        }
      }
    )

    # dynamic display, only show if results have been created
    output$ss_results_ui <- renderUI({
      if (reactive_values$show_results == TRUE) {
        span(
          layout_column_wrap(
            width = "200px",
            fixed_width = TRUE,
            actionButton(
              inputId = ns("copy_results"),
              label = "Copy results",
              class = "copy-button"
            ),
            actionButton(
              inputId = ns("copy_wl_results"),
              label = "Copy waiting list detail",
              class = "copy-button"
            )
          ),
          reactableOutput(
            ns("results_table")
          ),
          uiOutput(ns("plot_waiting_lists_ui"))
        )
      } else {
        p("Please make selections and generate results")
      }
    })
  })
}

## To be copied in the UI
# mod_08_batch_ui("08_batch_1")

## To be copied in the server
# mod_08_batch_server("08_batch_1")

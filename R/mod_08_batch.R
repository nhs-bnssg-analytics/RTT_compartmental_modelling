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
#'   sliderInput hr radioButtons conditionalPanel
#' @importFrom bslib input_task_button card card_header layout_sidebar sidebar
#'   layout_columns card_body page_fillable bs_theme
#' @importFrom lubridate years ceiling_date `%m+%`
#' @importFrom shinyWidgets pickerInput numericInputIcon
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
      multiple = TRUE,
      selected = sort(unique(org_lkp_ss_inputs$Region)),
      choicesOpt = list(
        class = rep("picker-class", length(unique(org_lkp_ss_inputs$Region)))
      )
    ),
    pickerInput(
      ns("selectedICBs"),
      "ICB(s):",
      choices = sort(unique(org_lkp_ss_inputs$ICB)),
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count",
        `count-selected-text` = "{0} Selections (on a total of {1})"
      ),
      selected = sort(unique(org_lkp_ss_inputs$ICB)),
      multiple = TRUE
    ),
    pickerInput(
      ns("selectedtrusts"),
      "Trust(s):",
      choices = sort(unique(org_lkp_ss_inputs$Trust)),
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count",
        `count-selected-text` = "{0} Selections (on a total of {1})",
        `live-search` = TRUE,
        `size` = 15 # Show max 15 items at once in dropdown
      ),
      selected = NULL,
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
    pickerInput(
      ns("specialty_codes"),
      "Specialties:",
      choices = sort(unname(treatment_function_codes)),
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count",
        `count-selected-text` = "{0} Selections (on a total of {1})",
        `live-search` = TRUE,
        `size` = 15 # Show max 15 items at once in dropdown
      ),
      selected = NULL,
      multiple = TRUE
    ),
    hr(),
    p(
      "Referral scenario (% annual change):",
      class = "referral-scenario-header"
    ),
    layout_columns(
      col_widths = c(6, 6),
      p("Low (optional):", class = "steady-state-body"),
      shinyWidgets::numericInputIcon(
        inputId = ns("referral_bin_low"),
        label = NULL,
        value = NULL,
        icon = list(NULL, icon("percent")),
        size = "sm"
      ),
      p("Medium:", class = "steady-state-body"),
      shinyWidgets::numericInputIcon(
        inputId = ns("referral_bin_medium"),
        label = NULL,
        value = 0,
        icon = list(NULL, icon("percent")),
        size = "sm"
      ),
      p("High (optional):", class = "steady-state-body"),
      shinyWidgets::numericInputIcon(
        inputId = ns("referral_bin_high"),
        label = NULL,
        value = NULL,
        icon = list(NULL, icon("percent")),
        size = "sm"
      )
    ),
    hr(),
    layout_columns(
      col_widths = c(6, 6),
      p(
        tooltip_label("Target date"),
        class = "steady-state-body"
      ),
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
        autoclose = TRUE
      ),
      p(
        tooltip_label("Target week"),
        class = "steady-state-body"
      ),
      shinyWidgets::numericInputIcon(
        inputId = ns("target_week"),
        label = NULL,
        min = 1,
        max = 52,
        value = 18,
        step = 1
      ),
      p(
        tooltip_label("Target proportion"),
        class = "steady-state-body"
      ),
      shinyWidgets::numericInputIcon(
        inputId = ns("target_value"),
        label = NULL,
        value = 92,
        min = 0,
        max = 100,
        step = 1,
        icon = list(NULL, icon("percent")),
        size = "sm"
      )
    ),
    hr(),
    bslib::input_task_button(
      id = ns("batch_run_rtt_data"),
      label = "Calculate steady state",
      label_busy = "Running...",
      type = "dark"
    ),
    hr(),
    p("Method settings"),
    radioButtons(
      inputId = ns("s_given_method"),
      label = HTML(paste(
        "Base the",
        tooltip_label("treatment profile"),
        "in the solution on a treatment profile seen in over the last 12 months, using the:"
      )),
      choices = list(
        Average = "mean",
        Median = "median",
        Latest = "latest"
      ),
      selected = "mean"
    ),
    layout_columns(
      col_widths = c(12),
      p(HTML(paste(
        "Base the",
        tooltip_label("renege proportions", "renege proportion"),
        "in the solution on:"
      ))),
      radioButtons(
        inputId = ns("renege_rate_option"),
        label = NULL,
        choices = list(
          "Recent historic rates" = "historic",
          "User input (between 0% and 100%)" = "user_input"
        ),
        selected = "historic"
      ),

      conditionalPanel(
        condition = "input.renege_rate_option == 'user_input'",
        ns = ns,
        shinyWidgets::numericInputIcon(
          inputId = ns("user_rate"),
          label = NULL,
          value = 10,
          min = 0,
          max = 100,
          step = 1,
          icon = list(NULL, icon("percent")),
          size = "sm"
        )
      )
    )
  )

  # Right Pane
  scenario_card <- card(
    card_header("Steady state results"),
    card_body(
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          title = "Description of methods",
          p(
            HTML(paste(
              "The results here represent healthy waiting list scenarios. This fulfils three criteria:",
              paste0(
                paste(
                  "<ol><li>Arrivals onto the waiting list are equal to departures, where arrivals are",
                  tooltip_label("referrals", "referral"),
                  "and departures are both",
                  tooltip_label("treatment capacity"),
                  "and",
                  tooltip_label("reneges.", "renege"),
                  "</li>"
                ),
                paste(
                  "<li>The calculated waiting list is achieving the desired",
                  tooltip_label("performance target.", "target proportion"),
                  "</li>"
                ),
                paste(
                  "<li>The proportion of",
                  tooltip_label("reneges", "renege"),
                  "that are departures is based on recent historic proportions.</li></ol>"
                )
              ),
              "The steps to identify the resulting solutions are as follows:",
              paste(
                "<ol><li>The final 12 months of available public data are used to understand:",
                paste0(
                  "<ul><li>on average, the proportion of people that ",
                  tooltip_label("renege"),
                  " who are on the waiting list, by how long they have been waiting.</li>",
                  "<li>the average/median/latest proportion of departures that were ",
                  tooltip_label("reneges.", "renege"),
                  "</li>",
                  paste(
                    "<li>the observed",
                    tooltip_label("treatment profile"),
                    "using the method specified by the user.</li></ul>"
                  )
                ),
                "</li>"
              ),
              "<li>These inputs, along with the projected referrals, are then optimised using a <a href='http://en.wikipedia.org/wiki/Linear_programming' target='_blank'>linear programming</a> method, finding a solution that minimises the difference between the treatment profile compared with the profile provided.</li>",
              paste0(
                "<li>If the resulting profile looks too different to the one provided, the ",
                tooltip_label("renege proportion"),
                " is incrementally reduced to allow for more realistic treatment profiles.</li></ol>"
              ),
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
#' @importFrom NHSRtt get_rtt_data find_p
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
          selected = current_icbs,
          choicesOpt = list(
            class = rep("picker-class", length(choicesI$ICB))
          )
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
          selected = current_provider,
          choicesOpt = list(
            class = rep("picker-class", length(choicesT$Trust))
          )
        )
      },
      ignoreInit = FALSE
    )

    # perform modelling when batch run selected -------------------------------

    observeEvent(
      c(input$batch_run_rtt_data),
      {
        if (input$batch_run_rtt_data > 0) {
          if (
            is.null(input$selectedtrusts) ||
              is.null(input$specialty_codes) ||
              all(
                is.null(input$referral_bin_low),
                is.null(input$referral_bin_medium),
                is.null(input$referral_bin_high)
              )
          ) {
            # If input is empty, show a modal dialog (popup)
            showModal(
              modalDialog(
                title = "Input Error",
                "Please make a selection for Trust, Specialty and one referrals scenario before submitting",
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

            # RDS VERSION
            raw_data <- readRDS(system.file(
              "extdata",
              "rtt_12months.rds",
              package = "RTTshiny"
            )) |>
              clean_raw_data() |>
              filter(trust %in% input$selectedtrusts) |>
              filter(specialty %in% c(input$specialty_codes))

            # calculate targets
            if (input$renege_rate_option == "historic") {
              targets <- raw_data |>
                calibrate_parameters(
                  max_months_waited = 12,
                  redistribute_m0_reneges = FALSE,
                  referrals_uplift = NULL,
                  full_breakdown = TRUE,
                  allow_negative_params = TRUE
                ) |>
                dplyr::select("trust", "specialty", "params") |>
                tidyr::unnest("params") |>
                dplyr::mutate(
                  reneges = case_when(
                    .data$reneges < 0 & .data$months_waited_id == 0 ~ 0,
                    .default = .data$reneges
                  )
                ) |>
                summarise(
                  renege_proportion = sum(.data$reneges) /
                    (sum(.data$reneges) + sum(.data$treatments)),
                  .by = c("trust", "specialty", "period_id")
                ) |>
                dplyr::mutate(
                  renege_proportion = case_when(
                    .data$renege_proportion < 0 ~ NA_real_,
                    .default = .data$renege_proportion
                  )
                ) |>
                summarise(
                  renege_proportion = stats::median(
                    .data$renege_proportion,
                    na.rm = TRUE
                  ),
                  .by = c("trust", "specialty")
                ) |>
                mutate(
                  renege_proportion = case_when(
                    is.na(.data$renege_proportion) ~ 0.15,
                    .default = .data$renege_proportion
                  )
                )
            } else {
              targets <- raw_data |>
                dplyr::distinct(.data$trust, .data$specialty) |>
                dplyr::mutate(renege_proportion = input$user_rate / 100)
            }

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
                ) |>
                  filter(!is.na(.data$referral_change))
              ) |>
              mutate(
                id = dplyr::row_number()
              )

            # calculate s profiles for the calibration period
            s_given <- calculate_s_given(
              data = raw_data,
              max_months_waited = 12,
              method = input$s_given_method
            )

            shiny::withProgress(
              message = "Processing trusts/specialties/scenarios",
              value = 0,
              {
                n <- nrow(current)
                # browser()
                optimised_projections <- current |>
                  # add historic s
                  left_join(
                    s_given,
                    by = c("trust", "specialty")
                  ) |>
                  # add historic renege rates by specialty
                  left_join(
                    targets |>
                      dplyr::select("trust", "specialty", "renege_proportion"),
                    by = c("trust", "specialty")
                  ) |>
                  # calculate steady state demand
                  mutate(
                    referrals_ss = .data$referrals_t1 +
                      ((.data$referrals_t1 * .data$referral_change / 100) *
                        forecast_months /
                        12),
                    target = .data$renege_proportion,
                    ss_calcs = purrr::pmap(
                      list(
                        ref_ss = .data$referrals_ss,
                        targ = .data$target,
                        par = .data$params,
                        s = .data$s_given,
                        id = .data$id
                      ),
                      \(ref_ss, targ, par, s, id) {
                        out <- append_steady_state(
                          referrals = ref_ss,
                          target = targ,
                          renege_params = par$renege_param,
                          percentile = input$target_value / 100,
                          target_time = input$target_week,
                          s_given = s,
                          method = "lp",
                          tolerance = 0.005
                        )

                        shiny::incProgress(
                          1 / n,
                          detail = paste("Completed combination", id, "of", n)
                        )

                        return(out)
                      }
                    )
                  ) |>
                  unnest("ss_calcs") |>
                  mutate(
                    current_vs_ss_wl_ratio = round(
                      .data$incompletes_t0 / .data$incompletes_ss,
                      2
                    ),
                    monthly_removals = (.data$incompletes_t0 -
                      .data$incompletes_ss) /
                      forecast_months,
                    referrals_scenario = gsub(
                      "_referrals",
                      "",
                      .data$referrals_scenario
                    )
                  )
              }
            )
            historic_waiting_list <- raw_data |>
              filter(
                .data$type %in% c("Incomplete", "Complete"),
                .data$period %in%
                  c(
                    max(.data$period),
                    max(.data$period) %m-% months(12)
                  )
              ) |>
              mutate(
                wl_description = paste(
                  format(.data$period, format = "%b %Y"),
                  "(observed)"
                ),
                type = case_when(
                  .data$type == "Incomplete" ~ "wlsize",
                  .data$type == "Complete" ~ "sigma",
                  .default = NA_character_
                )
              ) |>
              tidyr::pivot_wider(
                names_from = "type",
                values_from = "value",
                values_fill = 0
              ) |>
              select(
                "trust",
                "specialty",
                "period",
                "months_waited_id",
                "wlsize",
                "sigma",
                "wl_description"
              ) |>
              tidyr::nest(
                previous_waiting_list = c(
                  "period",
                  "months_waited_id",
                  "wlsize",
                  "wl_description",
                  "sigma"
                )
              )

            reactive_values$optimised_waiting_list <- optimised_projections |>
              select("trust", "specialty", "referrals_scenario", "wl_ss") |>
              tidyr::unnest("wl_ss") |>
              select(
                "trust",
                "specialty",
                "referrals_scenario",
                "months_waited_id",
                "wlsize",
                "sigma"
              ) |>
              mutate(
                wl_description = "Steady state (modelled)"
              ) |>
              tidyr::nest(
                steady_state_waiting_list = c(
                  "months_waited_id",
                  # "r",
                  # "service",
                  "sigma",
                  "wlsize",
                  "wl_description"
                )
              ) |>
              mutate(
                id = dplyr::row_number()
              ) |>
              left_join(
                historic_waiting_list,
                by = join_by(trust, specialty)
              )

            # add in the counterfactual reneges and wl size
            wl_t0 <- raw_data |>
              filter(
                .data$type == "Incomplete",
                .data$period == max(.data$period)
              ) |>
              select(!c("period", "period_id", "type")) |>
              rename(incompletes = "value") |>
              tidyr::nest(wl_t0 = c("months_waited_id", "incompletes"))

            shiny::withProgress(
              message = "Calculating counterfactuals",
              value = 0,
              {
                n <- nrow(optimised_projections)
                reactive_values$optimised_projections <- optimised_projections |>
                  left_join(wl_t0, by = c("trust", "specialty")) |>
                  mutate(
                    id = dplyr::row_number(),
                    counterfactual = purrr::pmap(
                      list(
                        cap = .data$capacity_t1,
                        ref_start = .data$referrals_t1,
                        ref_end = .data$referrals_ss,
                        inc = .data$wl_t0,
                        par = .data$params,
                        t = .data$trust,
                        sp = .data$specialty,
                        id = .data$id
                      ),
                      \(cap, ref_start, ref_end, inc, par, t, sp, id) {
                        counterf <- append_counterfactual(
                          capacity = cap,
                          referrals_start = ref_start,
                          referrals_end = ref_end,
                          incompletes_t0 = inc,
                          renege_capacity_params = par,
                          forecast_months = forecast_months,
                          target_week = input$target_week
                        )
                        shiny::incProgress(
                          1 / n,
                          detail = paste("Completed ", id, "of", n)
                        )
                        return(counterf)
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
                      "wl_t0",
                      "s_given",
                      "id"
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
                  ) |>
                  dplyr::relocate(
                    "referrals_scenario",
                    .after = "specialty"
                  )
              }
            )
            reactive_values$show_results <- TRUE
          }
        }
      }
    )

    output$plot_waiting_lists_ui <- renderUI({
      if (!is.null(reactive_values$optimised_waiting_list)) {
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
                        paste(
                          format(
                            max(.data$period, na.rm = TRUE) %m-% months(12),
                            format = "%b %Y"
                          ),
                          "(observed)"
                        ),
                        paste(
                          format(
                            max(.data$period, na.rm = TRUE),
                            format = "%b %Y"
                          ),
                          "(observed)"
                        ),
                        "Steady state (modelled)"
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
      }
    })

    # create the result table
    output$results_table <- reactable::renderReactable({
      if (reactive_values$show_results == TRUE) {
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
              header = name_with_tooltip("Trust", definition = "Trust name."),
              sticky = "left"
            ),
            specialty = colDef(
              header = name_with_tooltip(
                "Specialty",
                definition = "Specialty name."
              ),
              sticky = "left"
            ),
            referrals_scenario = colDef(
              header = name_with_tooltip(
                "Demand scenario",
                definition = "The demand scenario based on the user inputs."
              ),
              class = "divider-right",
              sticky = "left"
            ),
            referrals_t1 = colDef(
              header = name_with_tooltip(
                "Demand",
                definition = "The calculated current demand based on the previous 12 months"
              ),
              format = colFormat(digits = 1, separators = TRUE)
            ),
            capacity_t1 = colDef(
              header = name_with_tooltip(
                "Treatments",
                definition = "The calculated current treatment capacity based on the previous 12 months"
              ),
              format = colFormat(digits = 1, separators = TRUE)
            ),
            reneges_t0 = colDef(
              header = name_with_tooltip(
                "Reneges",
                definition = "Calculated total reneges for final month of 12 month calibration period"
              ),
              format = colFormat(digits = 1, separators = TRUE)
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
                definition = "The number of people on the waiting list for the last observed period."
              ),
              format = colFormat(digits = 0, separators = TRUE)
            ),
            pressure = colDef(
              header = name_with_tooltip(
                "Pressure",
                definition = "The current target percentile waiting time divided by the expected target percentilile wait time."
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
            referrals_counterf = colDef(
              header = name_with_tooltip(
                "Demand",
                definition = "The demand at the the target date based on the user inputs."
              ),
              format = colFormat(digits = 1, separators = TRUE)
            ),
            capacity_counterf = colDef(
              header = name_with_tooltip(
                "Treatments",
                definition = "The calculated number of monthly treatments under the 'do nothing' scenario at the target date."
              ),
              format = colFormat(digits = 1, separators = TRUE)
            ),
            reneges_counterf = colDef(
              header = name_with_tooltip(
                "Reneges",
                definition = "The calculated number of monthly reneges under the 'do nothing' scenario at the target date."
              ),
              format = colFormat(digits = 1, separators = TRUE)
            ),
            incompletes_counterf = colDef(
              header = name_with_tooltip(
                "Waiting list size",
                definition = "The number of people on the waiting list under the 'do nothing' scenario at the target date."
              ),
              format = colFormat(digits = 0, separators = TRUE)
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
                definition = "The demand at the the target date based on the user inputs."
              ),
              format = colFormat(digits = 1, separators = TRUE)
            ),
            capacity_ss = colDef(
              header = name_with_tooltip(
                "Treatments",
                definition = "The calculated number of monthly treatments to achieve the steady-state solution based on the selected method."
              ),
              format = colFormat(digits = 1, separators = TRUE)
            ),
            reneges_ss = colDef(
              header = name_with_tooltip(
                "Reneges",
                definition = "The calculated number of monthly reneges in the steady-state solution based on the selected method."
              ),
              format = colFormat(digits = 1, separators = TRUE)
            ),
            incompletes_ss = colDef(
              header = name_with_tooltip(
                "Waiting list size",
                definition = "The number of people on the waiting list in the steady-state solution based on the selected method."
              ),
              format = colFormat(digits = 0, separators = TRUE),
              class = "divider-right"
            ),
            current_vs_ss_wl_ratio = colDef(
              header = name_with_tooltip(
                "Current / steady state waiting list size",
                definition = "The ratio of the current waiting list size compared with the calculated steady-state waiting list size."
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
              format = colFormat(digits = 1),
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
                height = "450px",
                width = "1200px"
              )
            )
          },
          class = "steadystate-table"
        )
      } else {
        NULL
      }
    })

    # save WL results button -------------------------------------------------
    # Download handler
    output$download_results <- downloadHandler(
      filename <- paste0(
        "Steady-state results ",
        format(Sys.time(), format = "%Y%m%d %H%M%S"),
        ".csv"
      ),
      content = function(file) {
        reactive_values$optimised_projections |>
          dplyr::rename(
            dplyr::all_of(
              c(
                "Trust" = "trust",
                "Specialty" = "specialty",
                "Demand scenario" = "referrals_scenario",
                "Current demand" = "referrals_t1",
                "Current treatment capacity" = "capacity_t1",
                "Current reneges" = "reneges_t0",
                "Current load" = "load",
                "Current waiting list size" = "incompletes_t0",
                "Current pressure" = "pressure",
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
          utils::write.csv(file, row.names = FALSE)
      }
    )

    # save detailed WL results button -------------------------------------------------
    output$download_wl_results <- downloadHandler(
      filename <- paste0(
        "Steady-state waiting list detail ",
        format(Sys.time(), format = "%Y%m%d %H%M%S"),
        ".csv"
      ),
      content = function(file) {
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
          utils::write.csv(file, row.names = FALSE)
      }
    )

    # create PPT button ------------------------------------------------------

    output$create_ppt <- downloadHandler(
      filename <- paste0(
        "Steady-state results ",
        format(Sys.time(), format = "%Y%m%d %H%M%S"),
        ".pptx"
      ),
      content = function(file) {
        tempReport <- file.path(tempdir(), "skeleton.Rmd")

        file.copy(
          system.file(
            "rmarkdown",
            "templates",
            "steady-state",
            "skeleton",
            "skeleton.Rmd",
            package = "RTTshiny"
          ),
          tempReport,
          overwrite = TRUE
        )

        if (input$renege_rate_option == "historic") {
          report_renege_rate <- "historic"
        } else {
          report_renege_rate <- input$user_rate
        }

        params <- list(
          optimised_wl = reactive_values$optimised_waiting_list,
          optimised_projections = reactive_values$optimised_projections,
          target_week = input$target_week,
          target_value = input$target_value,
          settings = dplyr::tibble(
            "User input" = c(
              "Low referral scenario (% annual uplift)",
              "Medium referral scenario (% annual uplift)",
              "High referral scenario (% annual uplift)",
              "Target"
            ),
            "Value" = c(
              c(
                ifelse(
                  is.null(input$referral_bin_low),
                  NA,
                  input$referral_bin_low
                ),
                ifelse(
                  is.null(input$referral_bin_medium),
                  NA,
                  input$referral_bin_medium
                ),
                ifelse(
                  is.null(input$referral_bin_high),
                  NA,
                  input$referral_bin_high
                )
              ),
              paste0(
                input$target_value,
                "% waiting less than ",
                input$target_week,
                " weeks by ",
                format(input$target_date, "%d %b %Y")
              )
            )
          ) |>
            dplyr::filter(!is.na(.data$Value)),
          method = input$s_given_method,
          renege_rate = report_renege_rate
        )

        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )

        unlink(tempReport)
      }
    )

    # dynamic display, only show if results have been created
    output$ss_results_ui <- renderUI({
      if (reactive_values$show_results == TRUE) {
        span(
          layout_column_wrap(
            width = "200px",
            fixed_width = TRUE,
            downloadButton(
              outputId = ns("download_results"),
              label = "Download results as csv",
              class = "copy-button",
              icon = shiny::icon("file-csv")
            ),
            downloadButton(
              outputId = ns("download_wl_results"),
              label = "Download waiting list detail",
              class = "copy-button",
              icon = shiny::icon("hourglass-start")
            ),
            downloadButton(
              outputId = ns("create_ppt"),
              label = "Create PowerPoint of results",
              class = "ppt-button",
              icon = shiny::icon("file-powerpoint")
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

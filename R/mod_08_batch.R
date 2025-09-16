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
mod_08_batch_ui <- function(id) {
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
#'   summarise rename left_join join_by intersect all_of distinct slice
#' @importFrom reactable reactable renderReactable colDef colFormat reactableOutput
#'   colGroup
#' @noRd
mod_08_batch_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    reactive_values <- reactiveValues()
    reactive_values$show_results <- FALSE # determines whether outputs are shown
    reactive_values$optimised_projections <- NULL # these are the outputs
    reactive_values$optimised_waiting_list <- NULL # these are the outputs

    # trust selection filtering based on other NHS only checkbox ----------------------
    reactive_org_tbl <- reactiveVal(org_lkp)

    observeEvent(
      # reactively choose which trusts are displayed depending on the radio button selection
      c(input$ss_nhs_only),
      {
        reactive_org_tbl <- org_lkp

        if (input$ss_nhs_only == "nhs_only") {
          reactive_org_tbl <- reactive_org_tbl |>
            dplyr::filter(
              grepl("NHS", .data$`Provider Org Name`)
            )
        } else if (input$ss_nhs_only == "non_nhs_only") {
          reactive_org_tbl <- reactive_org_tbl |>
            dplyr::filter(
              !grepl("NHS", .data$`Provider Org Name`)
            )
        }

        # trust current selections
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
      }
    )

    # perform modelling when batch run selected -------------------------------

    observeEvent(
      c(input$batch_run_rtt_data),
      {
        if (input$batch_run_rtt_data > 0) {
          if (is.null(input$ss_trust_codes) || is.null(input$specialty_codes)) {
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
              trusts = input$ss_trust_codes,
              comm_parents = NA,
              comms = NA,
              spec = input$specialty_codes
            )

            # the latest month of data to use for calibrating the models
            max_download_date <- NHSRtt::latest_rtt_date()

            # min date is the 12th month prior to the latest month of data
            min_download_date <- lubridate::floor_date(
              max_download_date,
              unit = "months"
            ) %m-%
              months(12)

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
            ))

            # raw_data currently doesn't have a 1 to 1 relationship period-period_id because
            # some specialties have small numbers so they are missing, therefore
            # we must create a consistent lkp here

            period_lkp <- raw_data |>
              distinct(.data$period) |>
              arrange(.data$period) |>
              mutate(period_id = dplyr::row_number())

            raw_data <- raw_data |>
              select(!c("period_id")) |>
              left_join(period_lkp, by = join_by(period)) |>
              filter(trust %in% input$ss_trust_codes) |>
              filter(specialty %in% c(input$specialty_codes))

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

            shiny::withProgress(
              message = "Processing trusts/specialties/scenarios",
              value = 0,
              {
                n <- nrow(current)

                optimised_projections <- current |>
                  # add historic renege rates by specialty
                  left_join(
                    target_renege_proportions |>
                      dplyr::select("specialty", "renege_proportion"),
                    by = "specialty"
                  ) |>
                  # calculate steady state demand
                  mutate(
                    referrals_ss = .data$referrals_t1 +
                      ((.data$referrals_t1 * .data$referral_change / 100) *
                        forecast_months /
                        12),
                    target = case_when(
                      input$ss_method == "treatments" ~ .data$capacity_t1,
                      input$ss_method == "renege_rates" ~
                        .data$renege_proportion,
                      .default = NA_real_
                    ),
                    ss_calcs = purrr::pmap(
                      list(
                        ref_ss = .data$referrals_ss,
                        targ = .data$target,
                        par = .data$params,
                        id = .data$id
                      ),
                      \(ref_ss, targ, par, id) {
                        out <- append_steady_state(
                          referrals = ref_ss,
                          target = targ,
                          renege_params = par$renege_param,
                          percentile = input$target_value / 100,
                          target_time = input$target_week,
                          method = input$ss_method
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
                    wl_description = format(.data$period, format = "%b %Y"),
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
                    "months_waited_id",
                    "wlsize",
                    "sigma"
                  ) |>
                  mutate(
                    wl_description = "Steady state"
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

                reactive_values$optimised_projections <- optimised_projections |>
                  dplyr::select(
                    !c(
                      "params",
                      "referral_change",
                      "id",
                      "renege_proportion",
                      "target",
                      "wl_ss"
                    )
                  ) |>
                  distinct()
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
          "referrals_scenario",
          "referrals_ss",
          "capacity_ss",
          "reneges_ss",
          "incompletes_ss"
        )

        reactable::reactable(
          reactive_values$optimised_projections,
          columnGroups = list(
            colGroup(
              name = "Current",
              columns = current_cols
            ),
            colGroup(name = "Steady state", columns = ss_cols)
          ),
          filterable = TRUE,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 20, 50, 100),
          defaultPageSize = 10,
          defaultColDef = colDef(
            vAlign = "center",
            headerVAlign = "bottom",
            headerClass = "header"
          ),
          columns = list(
            trust = colDef(
              header = name_with_tooltip("Trust", definition = "Trust name")
            ),
            specialty = colDef(
              header = name_with_tooltip(
                "Specialty",
                definition = "Specialty name"
              ),
              class = "divider-right"
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
                        0.5
                      )
                    ),
                    midval = c(
                      "#FCFAFA" = 1
                    ),
                    highval = c(
                      "#FA7557" = max(
                        reactive_values$optimised_projections$load,
                        2
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
                        0.5
                      )
                    ),
                    midval = c(
                      "#FCFAFA" = 1
                    ),
                    highval = c(
                      "#FA7557" = max(
                        reactive_values$optimised_projections$pressure,
                        2
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
                        0.5
                      )
                    ),
                    midval = c(
                      "#FCFAFA" = 1
                    ),
                    highval = c(
                      "#FA7557" = max(
                        reactive_values$optimised_projections$current_vs_ss_wl_ratio,
                        2
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
                        -1
                      )
                    ),
                    midval = c(
                      "#FCFAFA" = 0
                    ),
                    highval = c(
                      "#FA7557" = max(
                        reactive_values$optimised_projections$monthly_removals,
                        2
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

    # dynamic display, only show if results have been created
    output$ss_results_ui <- renderUI({
      if (reactive_values$show_results == TRUE) {
        span(
          actionButton(
            inputId = ns("copy_results"),
            label = "Copy results",
            class = "copy-button"
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

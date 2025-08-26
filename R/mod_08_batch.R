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
      min = lubridate::ceiling_date(Sys.Date(), unit = "month"),
      max = lubridate::ceiling_date(Sys.Date() %m+% years(10), unit = "month"),
      value = "2029-03-01",
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
      col_widths = c(12),
      bslib::input_task_button(
        id = ns("batch_run_rtt_data"),
        label = "Batch Run",
        label_busy = "Running...",
        type = "dark"
      )
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
#' @importFrom shiny reactiveValues observeEvent renderUI helpText modalDialog modalButton
#'   tagList showModal
#' @importFrom NHSRtt latest_rtt_date get_rtt_data find_p
#' @importFrom lubridate floor_date interval
#' @importFrom rlang .data
#' @importFrom purrr map pmap list_rbind map2 map_dbl
#' @importFrom tidyr unnest nest complete
#' @importFrom dplyr filter mutate case_when select cross_join tibble row_number
#'   summarise rename left_join join_by intersect
#' @importFrom DT datatable renderDT DTOutput
#' @noRd
mod_08_batch_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    reactive_values <- reactiveValues()
    reactive_values$show_results <- FALSE # determines whether outputs are shown
    reactive_values$optimised_projections <- NULL # these are the outputs

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

            # # ORIGINAL CODE
            # raw_data <- seq(
            #   from = lubridate::floor_date(
            #     min_download_date, unit = "months"
            #   ),
            #   to = lubridate::floor_date(
            #     max_download_date, unit = "months"
            #   ),
            #   by = "months"
            # ) |>
            #   purrr::map(
            #     ~ NHSRtt::get_rtt_data(
            #       date_start = .x,
            #       date_end = .x,
            #       trust_parent_codes = NULL,
            #       trust_codes = selections_labels$trusts$selected_code,
            #       commissioner_parent_codes = NULL,
            #       commissioner_org_codes = NULL,
            #       specialty_codes = selections_labels$specialties$selected_code
            #     )
            #   ) |>
            #   purrr::list_rbind() |>
            #   aggregate_and_format_raw_data(
            #     selected_specialties = selections_labels$specialties$selected_name,
            #     min_date = min_download_date,
            #     max_date = max_download_date
            #   )

            # RDS VERSION
            raw_data <- readRDS(system.file(
              "extdata",
              "rtt_12months.rds",
              package = "RTTshiny"
            )) |>
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
            # browser()

            current <- append_current_status(
              data = raw_data,
              max_months_waited = 12
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
                id = dplyr::row_number()
              )

            shiny::withProgress(
              message = "Processing trusts/specialties/scenarios",
              value = 0,
              {
                n <- nrow(current)

                reactive_values$optimised_projections <- current |>
                  # calculate steady state demand
                  mutate(
                    referrals_ss = .data$referrals_t1 +
                      ((.data$referrals_t1 * .data$referral_change / 100) *
                        forecast_months /
                        12),
                    ss_calcs = purrr::pmap(
                      list(
                        par = .data$params,
                        ref_ss = .data$referrals_ss,
                        id = .data$id
                      ),
                      \(par, ref_ss, id) {
                        out <- append_steady_state(
                          params = par,
                          ss_demand = ref_ss
                        )

                        shiny::incProgress(
                          1 / n,
                          detail = paste("Completed combination", id, "of", n)
                        )

                        return(out)
                      }
                    )
                  ) |>
                  unnest(ss_calcs) |>
                  mutate(
                    current_vs_ss_wl_ratio = round(
                      .data$incompletes_t0 / .data$incompletes_ss,
                      2
                    ),
                    monthly_removals = (.data$incompletes_t0 -
                      .data$incompletes_ss) /
                      forecast_months
                  ) |>
                  dplyr::select(
                    !c(
                      "params",
                      "referral_change",
                      "id"
                    )
                  )
              }
            )

            reactive_values$show_results <- TRUE
          }
        }
      }
    )

    # create the result table
    output$results_table <- DT::renderDT({
      if (reactive_values$show_results == TRUE) {
        DT::datatable(
          reactive_values$optimised_projections,
          filter = "top",
          extensions = "Buttons",
          options = list(
            paging = TRUE,
            pageLength = 50,
            lengthMenu = c(25, 50, 100),
            searching = TRUE,
            ordering = TRUE,
            autoWidth = TRUE,
            dom = 'Blrtip',
            buttons = list(
              list(
                extend = 'copy',
                title = NULL, # prevents the title of the app being included when copying the data
                className = "dtButton",
                text = "Copy table to clipboard"
              ),
              list(
                extend = 'csv',
                className = 'dtButton',
                text = "Download table to csv"
              )
            )
          ),
          rownames = FALSE,
          colnames = c(
            "Trust" = "trust",
            "Specialty" = "specialty",
            "Current demand" = "referrals_t1",
            "Current treatment capacity" = "capacity_t1",
            "Current reneges" = "reneges_t0",
            "Current load" = "load",
            "Current waiting list size" = "incompletes_t0",
            "Demand scenario" = "referrals_scenario",
            "Steady state demand" = "referrals_ss",
            "Steady state treatment capacity" = "capacity_ss",
            "Steady state reneges" = "reneges_ss",
            "Steady state waiting list size" = "incompletes_ss",
            "Current / steady state waiting list size" = "current_vs_ss_wl_ratio",
            "Additional monthly removals required" = "monthly_removals"
          )
        ) |>
          DT::formatRound(
            columns = c(3:6, 9, 11, 14),
            digits = 1
          )
      } else {
        NULL
      }
    })

    # dynamic display, only show if results have been created
    output$ss_results_ui <- renderUI({
      if (reactive_values$show_results == TRUE) {
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

#' 03_results UI Function
#'
#' @description Module that displays the results
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList clickOpts uiOutput p actionButton plotOutput
#'   clickOpts
#' @importFrom DT DTOutput
#' @importFrom bslib navset_tab nav_panel card card_body page_sidebar
#'   layout_column_wrap tooltip
#' @importFrom dplyr if_else
#' @importFrom tidyr pivot_wider
mod_03_results_ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    sidebar = sidebar(
      uiOutput(ns("dynamic_sidebar_ui"))
    ),
    card(
      card_body(
        uiOutput(
          ns("results_ui")
        ),
        min_height = '60vh'
      )
    ),
    layout_column_wrap(
      uiOutput(
        ns("value_box_container")
      )
    )
  )
}

#' 03_results Server Functions
#' @importFrom DT renderDT formatRound datatable formatDate
#' @importFrom dplyr group_by rename summarise tribble
#' @importFrom rlang .data
#' @importFrom bslib value_box layout_column_wrap
#' @importFrom shiny updateActionButton showModal modalDialog downloadHandler
#'   actionButton p hr uiOutput modalButton removeModal
#' @importFrom shinyWidgets prettyCheckbox
#' @import ggplot2
#' @noRd
mod_03_results_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    reactive_data <- reactiveValues(
      plot_click_info = NULL,
      plot_clicked = NULL,
      btn_val = NULL,
      plot_data = NULL,
      show_plot = FALSE,
      show_table = FALSE,
      show_activity = FALSE,
      temp_data = NULL,
      facet_scales = "fixed",
      facet_grouping = "months_waited_id",
      calc_res = 96,
      activity_table = NULL
    )

    # dynamic sidebar ---------------------------------------------------------

    output$dynamic_sidebar_ui <- renderUI({
      if (is.null(r$chart_specification$scenario_type)) {
        tagList()
      } else if (
        r$chart_specification$scenario_type ==
          "Estimate performance (from treatment capacity inputs)"
      ) {
        layout_column_wrap(
          width = 1,
          gap = 0,
          p("INPUTS", class = "sidebar_header"),
          actionButton(
            inputId = ns("btn_referrals"),
            label = "Referrals",
            title = "Referral counts over the period",
            `data-bs-trigger` = "hover",
            icon = shiny::icon("star"),
            class = "results_button"
          ),
          p(
            "Treatment capacity",
            class = "results_button",
            title = "Treatment capacity counts over the period",
            `data-bs-trigger` = "hover",
          ),

          actionButton(
            inputId = ns("btn_capacity_ttl"),
            label = "Total",
            icon = shiny::icon("star"),
            class = "results_subbutton",
            `data-bs-trigger` = "hover",
            title = "Total counts of treatment capacity"
          ),
          actionButton(
            inputId = ns("btn_capacity_mnth"),
            label = "Distribution",
            icon = shiny::icon("star"),
            class = "results_subbutton",
            `data-bs-trigger` = "hover",
            title = "Treatment capacity distributed by number of months waited"
          ),
          hr(),
          p("OUTPUTS", class = "sidebar_header"),
          p(
            "Waiting list size",
            class = "results_button",
            `data-bs-trigger` = "hover",
            title = "The size of the waiting list over the period"
          ),
          actionButton(
            inputId = ns("btn_waiting_list_ttl"),
            label = "Total",
            icon = shiny::icon("star"),
            class = "results_subbutton",
            `data-bs-trigger` = "hover",
            title = "Total size of the waiting list"
          ),
          actionButton(
            inputId = ns("btn_waiting_list_mnth"),
            label = "Distribution",
            icon = shiny::icon("star"),
            class = "results_subbutton",
            `data-bs-trigger` = "hover",
            title = "Size of the waiting list by the number of months waited"
          ),
          actionButton(
            inputId = ns("btn_performance"),
            label = "18 week performance",
            icon = shiny::icon("star"),
            class = "results_button",
            `data-bs-trigger` = "hover",
            title = "The 18 week performance over the period"
          ),
          actionButton(
            inputId = ns("btn_shortfall"),
            label = "Performance shortfall",
            icon = shiny::icon("star"),
            class = "results_button",
            `data-bs-trigger` = "hover",
            title = "The number of additional long waiters that, when removed, would results in achieving performance"
          ),
          p(
            "Reneges",
            class = "results_button",
            `data-bs-trigger` = "hover",
            title = "Number of reneges over the period"
          ),
          actionButton(
            inputId = ns("btn_reneges_ttl"),
            label = "Total",
            icon = shiny::icon("star"),
            class = "results_subbutton",
            `data-bs-trigger` = "hover",
            title = "The total number of reneges each month"
          ),
          actionButton(
            inputId = ns("btn_reneges_mnth"),
            label = "Distribution",
            icon = shiny::icon("star"),
            class = "results_subbutton",
            `data-bs-trigger` = "hover",
            title = "Reneges distributed by number of months waited"
          ),
          actionButton(
            inputId = ns("btn_activity"),
            label = "Activity (experimental)",
            icon = shiny::icon("star"),
            class = "results_button",
            `data-bs-trigger` = "hover",
            title = "Projected activity based on the clock stops"
          ),
          hr(),
          actionButton(
            inputId = ns("btn_data"),
            label = "Data table",
            icon = shiny::icon("star"),
            class = "results_button",
            `data-bs-trigger` = "hover",
            title = "View the data in a table"
          ),
          uiOutput(ns("btn_report_ui"))
        )
      } else if (
        r$chart_specification$scenario_type ==
          "Estimate treatment capacity (from performance targets)"
      ) {
        layout_column_wrap(
          width = 1,
          gap = 0,
          p("INPUTS", class = "sidebar_header"),
          actionButton(
            inputId = ns("btn_referrals"),
            label = "Referrals",
            title = "Referral counts over the period",
            `data-bs-trigger` = "hover",
            icon = shiny::icon("star"),
            class = "results_button"
          ),
          actionButton(
            inputId = ns("btn_performance"),
            label = "18 week performance",
            icon = shiny::icon("star"),
            class = "results_button",
            `data-bs-trigger` = "hover",
            title = "The 18 week performance over the period"
          ),
          hr(),
          p("OUTPUTS", class = "sidebar_header"),
          p(
            "Treatment capacity",
            class = "results_button",
            title = "Treatment capacity counts over the period",
            `data-bs-trigger` = "hover",
          ),

          actionButton(
            inputId = ns("btn_capacity_ttl"),
            label = "Total",
            icon = shiny::icon("star"),
            class = "results_subbutton",
            `data-bs-trigger` = "hover",
            title = "Total counts of treatment capacity"
          ),
          actionButton(
            inputId = ns("btn_capacity_mnth"),
            label = "Distribution",
            icon = shiny::icon("star"),
            class = "results_subbutton",
            `data-bs-trigger` = "hover",
            title = "Treatment capacity distributed by number of months waited"
          ),
          p(
            "Reneges",
            class = "results_button",
            `data-bs-trigger` = "hover",
            title = "Number of reneges over the period"
          ),
          actionButton(
            inputId = ns("btn_reneges_ttl"),
            label = "Total",
            icon = shiny::icon("star"),
            class = "results_subbutton",
            `data-bs-trigger` = "hover",
            title = "The total number of reneges each month"
          ),
          actionButton(
            inputId = ns("btn_reneges_mnth"),
            label = "Distribution",
            icon = shiny::icon("star"),
            class = "results_subbutton",
            `data-bs-trigger` = "hover",
            title = "Reneges distributed by number of months waited"
          ),
          p(
            "Waiting list size",
            class = "results_button",
            `data-bs-trigger` = "hover",
            title = "The size of the waiting list over the period"
          ),
          actionButton(
            inputId = ns("btn_waiting_list_ttl"),
            label = "Total",
            icon = shiny::icon("star"),
            class = "results_subbutton",
            `data-bs-trigger` = "hover",
            title = "Total size of the waiting list"
          ),
          actionButton(
            inputId = ns("btn_waiting_list_mnth"),
            label = "Distribution",
            icon = shiny::icon("star"),
            class = "results_subbutton",
            `data-bs-trigger` = "hover",
            title = "Size of the waiting list by the number of months waited"
          ),
          actionButton(
            inputId = ns("btn_activity"),
            label = "Activity (experimental)",
            icon = shiny::icon("star"),
            class = "results_button",
            `data-bs-trigger` = "hover",
            title = "Projected activity based on the clock stops"
          ),
          hr(),
          actionButton(
            inputId = ns("btn_data"),
            label = "Data table",
            icon = shiny::icon("star"),
            class = "results_button",
            `data-bs-trigger` = "hover",
            title = "View the data in a table"
          ),
          uiOutput(ns("btn_report_ui"))
        )
      }
    })
    # report download ---------------------------------------------------------

    output$btn_report_ui <- renderUI({
      if (!is.null(r$waiting_list)) {
        if (!requireNamespace("flextable", quietly = TRUE)) {
          showModal(
            modalDialog(
              title = "flextable missing",
              "The 'flextable' package is required to enable reporting functionality. If you would like this, please exit the app and run 'install.packages('flextable')'.",
              easyClose = TRUE,
              footer = NULL
            )
          )
        } else {
          downloadButton(
            ns("btn_report"),
            "Generate report" #,
            # style = "width:25%;"
          )
        }
      }
    })

    output$btn_report <- downloadHandler(
      filename <- paste0(
        r$chart_specification$trust,
        " ",
        r$chart_specification$specialty,
        " ",
        format(Sys.time(), format = "%Y%m%d %H%M%S"),
        ".docx"
      ),
      content = function(file) {
        tempReport <- file.path(tempdir(), "skeleton.Rmd")

        file.copy(
          system.file(
            "rmarkdown",
            "templates",
            "scenario-report",
            "skeleton",
            "skeleton.Rmd",
            package = "RTTshiny"
          ),
          tempReport,
          overwrite = TRUE
        )

        params <- list(
          waiting_list = r$waiting_list,
          trust = r$chart_specification$trust,
          specialty = r$chart_specification$specialty,
          scenario = r$chart_specification$scenario_type,
          cap_change = r$chart_specification$capacity_percent_change,
          cap_skew = r$chart_specification$capacity_skew,
          cap_change_type = r$chart_specification$capacity_change_type,
          target_data = r$chart_specification$target_data,
          referrals_percent_change = r$chart_specification$referrals_percent_change,
          referrals_change_type = r$chart_specification$referrals_change_type,
          chart_specification = r$chart_specification,
          facet_scales = reactive_data$facet_scales,
          facet_grouping = reactive_data$facet_grouping
        )
        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )

    # identify last clicked button --------------------------------------------

    lapply(
      X = c(
        "referrals",
        "capacity_ttl",
        "capacity_mnth",
        "reneges_ttl",
        "reneges_mnth",
        "waiting_list_ttl",
        "waiting_list_mnth",
        "performance",
        "shortfall",
        "activity",
        "data",
        "report_ui"
      ),
      FUN = function(i) {
        observeEvent(input[[paste0("btn_", i)]], {
          if (input[[paste0("btn_", i)]] > 0) {
            if (!is.null(reactive_data$btn_val)) {
              shiny::updateActionButton(
                session = session,
                inputId = reactive_data$btn_val,
                icon = shiny::icon("star")
              )
            }

            reactive_data$btn_val <- paste0("btn_", i)
            shiny::updateActionButton(
              session = session,
              inputId = reactive_data$btn_val,
              icon = shiny::icon("star", class = "fa-solid fa-star")
            )
          }
          reactive_data$plot_clicked <- FALSE

          if (reactive_data$btn_val == "btn_data") {
            reactive_data$show_plot <- FALSE
            reactive_data$show_table <- TRUE
            reactive_data$show_activity <- FALSE
          } else if (reactive_data$btn_val == "btn_activity") {
            reactive_data$show_plot <- FALSE
            reactive_data$show_table <- FALSE
            reactive_data$show_activity <- TRUE
          } else if (reactive_data$btn_val != "btn_report_ui") {
            reactive_data$show_plot <- TRUE
            reactive_data$show_table <- FALSE
            reactive_data$show_activity <- FALSE
          }
        })
      }
    )

    # DT table ----------------------------------------------------------------

    output$results_table <- DT::renderDT(
      {
        new_col_names <- c(
          "Months waited" = "months_waited_id",
          "Calculated treatment capacity" = "calculated_treatments",
          Reneges = "reneges",
          "Waiting list size (at end of month)" = "incompletes",
          "Unadjusted referrals" = "unadjusted_referrals",
          "Adjusted referrals" = "adjusted_referrals",
          "Treatment capacity skew" = "capacity_skew",
          "Measure type" = "period_type",
          "Month start date" = "period"
        )

        if (is.null(r$waiting_list)) {
          dplyr::tibble(
            period = NA,
            months_waited_id = NA,
            calculated_treatments = NA,
            reneges = NA,
            incompletes = NA,
            unadjusted_referrals = NA,
            adjusted_referrals = NA,
            capacity_skew = NA,
            period_type = NA
          ) |>
            DT::datatable(
              colnames = new_col_names,
              rownames = FALSE,
              caption = "Please return to the 'Scenario planner' tab to create some modelled data",
              options = list(
                dom = 't', # 't' means show only the table, no other elements
                paging = FALSE, # Disable pagination
                searching = FALSE, # Disable search box
                info = FALSE # Remove "Showing X of Y entries" text
              )
            )
        } else {
          r$waiting_list |>
            dplyr::select(
              !c("period_id")
            ) |>
            dplyr::relocate(
              "period",
              .before = dplyr::everything()
            ) |>
            DT::datatable(
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
                    text = "Download table to csv",
                    title = paste0(
                      r$chart_specification$trust,
                      " ",
                      r$chart_specification$specialty,
                      " ",
                      format(Sys.time(), format = "%Y%m%d %H%M%S")
                    )
                  )
                )
              ),
              colnames = new_col_names,
              rownames = FALSE
            ) |>
            DT::formatRound(
              columns = c(
                "Calculated treatment capacity",
                "Reneges",
                "Waiting list size (at end of month)",
                "Unadjusted referrals",
                "Adjusted referrals"
              ),
              digits = 1
            )
        }
      },
      server = FALSE
    )

    # update reactive scales object ------------------------------------------

    observeEvent(
      c(input$facet_scales),
      {
        if (isTRUE(input$facet_scales) | is.null(input$facet_scales)) {
          reactive_data$facet_scales <- "fixed"
        } else {
          reactive_data$facet_scales <- "free_y"
        }
      }
    )

    # update reactive grouping object ----------------------------------------
    observeEvent(
      c(input$facet_grouping),
      {
        reactive_data$facet_grouping <- input$facet_grouping
      }
    )

    # plot --------------------------------------------------------------------
    observeEvent(
      c(
        input$chart_res,
        input$btn_referrals,
        input$btn_capacity_ttl,
        input$btn_capacity_mnth,
        input$btn_reneges_ttl,
        input$btn_reneges_mnth,
        input$btn_waiting_list_ttl,
        input$btn_waiting_list_mnth,
        input$btn_performance,
        input$btn_shortfall,
        input$btn_activity,
        input$btn_data,
        input$btn_report_ui
      ),
      {
        if (!is.null(input$chart_res)) {
          reactive_data$calc_res <- input$chart_res
        } else if (is.null(reactive_data$calc_res)) {
          reactive_data$calc_res <- 96
        }
        output$results_plot <- renderPlot(
          {
            if (
              is.null(r$waiting_list) |
                identical(r$waiting_list, tibble())
            ) {
              holding_chart(type = "model")
            } else if (is.null(reactive_data$btn_val)) {
              holding_chart(type = "select_chart")
            } else {
              if (reactive_data$btn_val == "btn_referrals") {
                reactive_data$plot_data <- r$waiting_list |>
                  dplyr::filter(.data$months_waited_id == "0-1 months") |>
                  dplyr::mutate(
                    p_var = sum(.data$adjusted_referrals),
                    .by = c("period", "period_type")
                  ) |>
                  extend_period_type_data()

                chart_type <- "referrals"
                include_facets <- FALSE
                percentage_axis <- FALSE
                include_target_line <- FALSE
                chart_facet_grouping <- NULL
              } else if (reactive_data$btn_val == "btn_capacity_ttl") {
                reactive_data$plot_data <- r$waiting_list |>
                  dplyr::summarise(
                    p_var = sum(.data$calculated_treatments, na.rm = T),
                    .by = c("period", "period_type")
                  ) |>
                  extend_period_type_data()

                chart_type <- "total treatment capacity"
                include_facets <- FALSE
                percentage_axis <- FALSE
                include_target_line <- FALSE
                chart_facet_grouping <- NULL
              } else if (reactive_data$btn_val == "btn_capacity_mnth") {
                reactive_data$plot_data <- r$waiting_list |>
                  dplyr::summarise(
                    p_var = sum(.data$calculated_treatments, na.rm = T),
                    .by = c("period", "period_type", "months_waited_id")
                  ) |>
                  extend_period_type_data()

                chart_type <- "treatment capacity by months waiting"
                include_facets <- TRUE
                percentage_axis <- FALSE
                include_target_line <- FALSE
                chart_facet_grouping <- reactive_data$facet_grouping
              } else if (reactive_data$btn_val == "btn_reneges_ttl") {
                reactive_data$plot_data <- r$waiting_list |>
                  dplyr::summarise(
                    p_var = sum(.data$reneges, na.rm = T),
                    .by = c("period", "period_type")
                  ) |>
                  extend_period_type_data()

                chart_type <- "total net reneges"
                include_facets <- FALSE
                percentage_axis <- FALSE
                include_target_line <- FALSE
                chart_facet_grouping <- NULL
              } else if (reactive_data$btn_val == "btn_reneges_mnth") {
                reactive_data$plot_data <- r$waiting_list |>
                  dplyr::summarise(
                    p_var = sum(.data$reneges, na.rm = T),
                    .by = c("period", "period_type", "months_waited_id")
                  ) |>
                  extend_period_type_data()

                chart_type <- "net reneges by months waiting"
                include_facets <- TRUE
                percentage_axis <- FALSE
                include_target_line <- FALSE
                chart_facet_grouping <- reactive_data$facet_grouping
              } else if (reactive_data$btn_val == "btn_waiting_list_ttl") {
                reactive_data$plot_data <- r$waiting_list |>
                  dplyr::summarise(
                    p_var = sum(.data$incompletes, na.rm = T),
                    .by = c("period", "period_type")
                  ) |>
                  extend_period_type_data()

                chart_type <- "waiting list size"
                include_facets <- FALSE
                percentage_axis <- FALSE
                include_target_line <- FALSE
                chart_facet_grouping <- NULL
              } else if (reactive_data$btn_val == "btn_waiting_list_mnth") {
                reactive_data$plot_data <- r$waiting_list |>
                  dplyr::mutate(p_var = .data$incompletes) |>
                  extend_period_type_data()

                chart_type <- "numbers waiting by period"
                include_facets <- TRUE
                percentage_axis <- FALSE
                include_target_line <- FALSE
                chart_facet_grouping <- reactive_data$facet_grouping
              } else if (reactive_data$btn_val == "btn_performance") {
                reactive_data$plot_data <- r$waiting_list |>
                  dplyr::rename(value = "incompletes") |>
                  dplyr::group_by(.data$period_type) |>
                  mutate(
                    months_waited_id = extract_first_number(
                      .data$months_waited_id
                    )
                  ) |>
                  calc_performance(
                    target_bin = 4
                  ) |>
                  ungroup() |>
                  rename(p_var = "prop") |>
                  extend_period_type_data()

                chart_type <- "18 weeks performance"
                include_facets <- FALSE
                percentage_axis <- TRUE
                include_target_line <- TRUE
                chart_facet_grouping <- NULL
              } else if (reactive_data$btn_val == "btn_shortfall") {
                if (is.null(input$shortf_targ_bin)) {
                  targ_bin <- 4
                } else {
                  targ_bin <- input$shortf_targ_bin
                }

                if (is.null(input$shortfall_target_value)) {
                  targ_prop <- 0.92
                } else {
                  targ_prop <- input$shortfall_target_value / 100
                }

                reactive_data$plot_data <- r$waiting_list |>
                  dplyr::rename(value = "incompletes") |>
                  dplyr::group_by(.data$period_type) |>
                  mutate(
                    months_waited_id = extract_first_number(
                      .data$months_waited_id
                    )
                  ) |>
                  calc_shortfall(
                    target_bin = targ_bin,
                    target_performance = targ_prop
                  ) |>
                  ungroup() |>
                  rename(p_var = "shortfall") |>
                  extend_period_type_data()

                chart_type <- paste0(
                  "Performance shortfall (",
                  round(targ_prop * 100, 1),
                  "% waiting less than ",
                  targ_bin,
                  " months)"
                )
                include_facets <- FALSE
                percentage_axis <- FALSE
                include_target_line <- FALSE
                chart_facet_grouping <- NULL
              }

              if (
                !(reactive_data$btn_val %in%
                  c("btn_data", "btn_report_ui", "btn_activity"))
              ) {
                plot_output(
                  data = reactive_data$plot_data,
                  p_trust = r$chart_specification$trust,
                  p_speciality = r$chart_specification$specialty,
                  p_chart = chart_type,
                  p_scenario = r$chart_specification$scenario_type,
                  p_cap_change = r$chart_specification$capacity_percent_change,
                  p_cap_skew = r$chart_specification$capacity_skew,
                  p_cap_change_type = r$chart_specification$capacity_change_type,
                  p_target_data = r$chart_specification$target_data,
                  p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                  p_referrals_change_type = r$chart_specification$referrals_change_type,
                  p_perc = percentage_axis,
                  p_facet = include_facets,
                  p_target_line = include_target_line,
                  p_facet_scales = reactive_data$facet_scales,
                  p_facet_grouping = chart_facet_grouping,
                  data_source = r$chart_specification$data_source
                )
              }
            }
          },
          res = reactive_data$calc_res
        )
      }
    )

    # dynamic ui --------------------------------------------------------------
    output$results_ui <- renderUI({
      if (reactive_data$show_table == TRUE) {
        final_ui <- DTOutput(
          ns("results_table")
        )
      } else if (reactive_data$show_activity == TRUE) {
        clock_stops <- r$waiting_list |>
          dplyr::filter(.data$period_type == "Projected") |>
          dplyr::summarise(
            dplyr::across(
              c("calculated_treatments", "reneges"),
              sum
            )
          )

        output$activity_table <- reactable::renderReactable({
          activity_table <- setNames(
            clock_stops$calculated_treatments + clock_stops$reneges,
            nm = r$chart_specification$specialty
          ) |>
            convert_clock_stops_to_activity() |>
            purrr::pluck(1)

          if (!"Unable to compute" %in% names(activity_table)) {
            activity_lkp <- dplyr::tibble(
              type = c(
                rep("Outpatient", 4),
                rep("Inpatient", 2)
              ),
              metric_abb = c(
                "avg_op_first_activity_per_pathway_op_only",
                "avg_op_flup_activity_per_pathway_op_only",
                "avg_op_first_activity_per_pathway_mixed",
                "avg_op_flup_activity_per_pathway_mixed",
                "ip_daycase_count",
                "ip_non_daycase_count"
              ),
              metric = c(
                "First attendances",
                "Follow up attendances",
                "First attendances",
                "Follow up attendances",
                "Day case admissions",
                "Non-day case admissions"
              )
            )

            activity_table <- activity_table |>
              tidyr::pivot_longer(
                cols = !c("treatment_function"),
                names_to = "metric_abb",
                values_to = "count"
              ) |>
              left_join(activity_lkp, by = "metric_abb") |>
              select(c("treatment_function", "type", "metric", "count")) |>
              dplyr::summarise(
                count = sum(.data$count),
                .by = c("treatment_function", "type", "metric")
              ) |>
              mutate(
                count = round(.data$count, 0),
                proportion = .data$count / sum(.data$count),
                .by = "type"
              )

            reactive_data$activity_table <- activity_table

            activity_table_ui <- reactable::reactable(
              reactive_data$activity_table,
              filterable = FALSE,
              showPageSizeOptions = FALSE,
              fullWidth = FALSE,
              width = 850,
              defaultPageSize = 10,
              defaultColDef = colDef(
                vAlign = "center",
                headerVAlign = "bottom",
                headerClass = "header"
              ),
              columns = list(
                treatment_function = colDef(
                  header = name_with_tooltip(
                    "Specialty",
                    definition = "Treatment specialty."
                  )
                ),
                type = colDef(
                  header = name_with_tooltip(
                    "Pathway type",
                    definition = "The type of pathway that the activity refers to."
                  )
                ),
                metric = colDef(
                  header = name_with_tooltip(
                    "Activity metric",
                    definition = "Activity metric."
                  )
                ),
                count = colDef(
                  header = name_with_tooltip(
                    "Count",
                    definition = "Count of activity in the projected period."
                  ),
                  format = colFormat(separators = TRUE)
                ),
                proportion = colDef(
                  header = name_with_tooltip(
                    "Proportion of pathway type",
                    definition = "Proportion of pathway type that activity comprises in the projected period."
                  ),
                  format = colFormat(percent = TRUE, digits = 1)
                )
              )
            )
          } else {
            NULL
          }
        })

        output$copy_to_clipboard <- renderUI({
          if (sum(reactive_data$activity_table$count, na.rm = TRUE) != 0) {
            actionButton(
              inputId = ns("copy_btn"),
              label = "Copy results to clipboard",
              icon = shiny::icon("clipboard"),
              class = "copy-button",
              width = "300px"
            )
          } else {
            NULL
          }
        })

        final_ui <- card_body(
          p(
            "NHS England analysis from 2022 calculated average associated activity with all clock stops, by specialty, at a national level."
          ),
          p(
            "This analysis is applied to the projected period from this scenario to provide an overall indication of quantity and type of activity for the projected period."
          ),
          p(
            "This provides no indication of when the activity should take place, as some of it may occur in the months before or after the projected period."
          ),
          p(
            paste(
              "This table shows the estimated activity associated with the combined",
              formatC(
                clock_stops$calculated_treatments,
                big.mark = ",",
                digits = 0,
                format = "f"
              ),
              "treatments and",
              formatC(
                clock_stops$reneges,
                big.mark = ",",
                digits = 0,
                format = "f"
              ),
              "reneges for the projected period."
            )
          ),
          uiOutput(ns("copy_to_clipboard")),
          reactable::reactableOutput(ns("activity_table"))
        )
      } else {
        # if no button has been selected then display results_plot
        if (is.null(reactive_data$btn_val)) {
          final_ui <- plotOutput(
            ns("results_plot"),
            click = shiny::clickOpts(
              id = ns("plot_click")
            ),
            height = "600px"
          )
        } else {
          # if calculating performance from capacity inputs then we need to show the editing options

          plot <- plotOutput(
            ns("results_plot"),
            click = shiny::clickOpts(
              id = ns("plot_click")
            ),
            height = "600px"
          )

          res <- div(
            class = "label-left",
            sliderInput(
              inputId = ns("chart_res"),
              label = "Select chart resolution (pixels per inch)",
              min = 72,
              max = 144,
              value = reactive_data$calc_res,
              step = 8
            )
          )

          if (reactive_data$facet_scales == "fixed") {
            axis_start_val <- TRUE
          } else {
            axis_start_val <- FALSE
          }

          distribution_options <- card(layout_columns(
            col_widths = c(6, 6),
            shiny::radioButtons(
              inputId = ns("facet_grouping"),
              label = "Chart grouping",
              choices = c(
                "Months waited" = "months_waited_id",
                "Time" = "period"
              ),
              selected = reactive_data$facet_grouping,
              inline = FALSE
            ),
            shinyWidgets::prettyCheckbox(
              inputId = ns("facet_scales"),
              label = "Fixed y-axis",
              value = axis_start_val,
              inline = FALSE
            )
          ))

          if (
            r$chart_specification$scenario_type ==
              "Estimate performance (from treatment capacity inputs)"
          ) {
            edit <- actionButton(
              ns("edit_data"),
              "Edit input data",
              class = "btn-primary"
            )
            if (reactive_data$btn_val == "btn_shortfall") {
              shortfall_options <- card(
                layout_columns(
                  col_widths = c(8, 4),
                  span(
                    "Number of months on the waiting list below which the performance target applies:"
                  ),
                  numericInput(
                    inputId = ns("shortf_targ_bin"),
                    label = NULL,
                    value = 4,
                    min = 1,
                    max = 12,
                    step = 1
                  ),
                  span(
                    "Performance target:"
                  ),
                  shinyWidgets::numericInputIcon(
                    inputId = ns("shortfall_target_value"),
                    label = NULL,
                    min = 0,
                    max = 100,
                    value = 92,
                    icon = list(NULL, shiny::icon("percent")),
                    size = "sm"
                  )
                )
              )
              final_ui <- div(
                plot,
                layout_columns(
                  col_widths = c(3, -5, 4),
                  card(res, edit),
                  shortfall_options
                )
              )
            } else if (grepl("_mnth$", reactive_data$btn_val)) {
              final_ui <- div(
                plot,
                layout_columns(
                  col_widths = c(3, -5, 4),
                  card(res, edit),
                  distribution_options
                )
              )
            } else {
              final_ui <- div(
                plot,
                layout_columns(
                  col_widths = c(3, -9),
                  card(res, edit)
                )
              )
            }
          } else if (
            r$chart_specification$scenario_type ==
              "Estimate treatment capacity (from performance targets)"
          ) {
            if (grepl("_mnth$", reactive_data$btn_val)) {
              final_ui <- div(
                plot,
                layout_columns(
                  col_widths = c(3, -5, 4),
                  card(res),
                  distribution_options
                )
              )
            } else {
              final_ui <- div(
                plot,
                layout_columns(
                  col_widths = c(3, -9),
                  card(res)
                )
              )
            }
          }
        }
      }
      final_ui
    })

    # copy results
    observeEvent(
      input$copy_btn,
      {
        if (input$copy_btn > 0) {
          utils::write.table(
            reactive_data$activity_table,
            file = "clipboard",
            sep = "\t",
            row.names = FALSE
          )
        }
      }
    )

    # Show modal when edit button is clicked
    observeEvent(input$edit_data, {
      reactive_data$temp_data <- r$waiting_list |>
        dplyr::filter(
          .data$period_type == "Projected"
        ) |>
        dplyr::summarise(
          across(
            c("adjusted_referrals", "calculated_treatments"),
            ~ round(sum(.x, na.rm = TRUE), 2)
          ),
          .by = c("period")
        )

      showModal(
        modalDialog(
          title = "Edit Inputs",
          size = "l",
          DTOutput(
            ns("data_table")
          ),
          footer = tagList(
            actionButton(
              ns("save_changes"),
              "Save Changes",
              class = "btn-success"
            ),
            actionButton(
              ns("reset_data"),
              "Reset",
              class = "btn-secondary"
            ),
            modalButton("Cancel")
          )
        )
      )
    })

    # Render editable data table
    output$data_table <- renderDT({
      datatable(
        reactive_data$temp_data,
        editable = list(
          target = "cell",
          disable = list(
            columns = 0 # indices here start at 0 for column 1
          ), # disable editing the first column
          numeric = 2:3 # allow only numeric values - indices here start at 1 for column 1
        ),
        options = list(
          pageLength = 15,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        colnames = c(
          "Month start date",
          "Referrals",
          "Treatment capacity"
        ),
        caption = "Double-click on a cell to edit the value"
      ) |>
        DT::formatRound(
          columns = 2:3
        ) |>
        DT::formatDate(
          columns = 1,
          method = 'toLocaleDateString',
          params = list('fr-FR')
        )
    })

    # Handle table edits
    observeEvent(input$data_table_cell_edit, {
      info <- input$data_table_cell_edit
      # Create a copy of the data to avoid reference issues
      temp_copy <- reactive_data$temp_data

      # Update the specific cell
      temp_copy[info$row, info$col + 1] <- as.numeric(info$value)

      # Update the reactive value
      reactive_data$temp_data <- temp_copy |>
        dplyr::mutate(
          across(
            c("adjusted_referrals", "calculated_treatments"),
            \(x) ifelse(x < 0, 0, x)
          )
        )
    })

    # Save changes
    observeEvent(input$save_changes, {
      removeModal()

      r$waiting_list <- calculate_customised_projections(
        original_wl_data = r$chart_specification$original_data$waiting_list,
        new_referrals_capacity = reactive_data$temp_data,
        original_params = r$chart_specification$params,
        surplus_capacity_option = r$surplus_capacity_option
      )

      # pass information in the charts
      r$chart_specification$referrals_percent_change <- ""
      r$chart_specification$referrals_change_type <- "manual"
      r$chart_specification$scenario_type <- "Estimate performance (from treatment capacity inputs)"
      r$chart_specification$capacity_percent_change <- ""
      r$chart_specification$capacity_change_type <- "manually adjusted"
    })

    # Reset to original data
    observeEvent(input$reset_data, {
      original_referrals_capacity <- r$chart_specification$original_data$waiting_list |>
        filter(
          .data$period_type == "Projected"
        ) |>
        summarise(
          across(
            c("adjusted_referrals", "calculated_treatments"),
            ~ sum(.x, na.rm = TRUE)
          ),
          .by = c("period")
        )

      removeModal()

      r$waiting_list <- calculate_customised_projections(
        original_wl_data = r$chart_specification$original_data$waiting_list,
        new_referrals_capacity = original_referrals_capacity,
        original_params = r$chart_specification$params,
        surplus_capacity_option = r$surplus_capacity_option
      )

      # pass information in the charts
      r$chart_specification$referrals_percent_change <- r$chart_specification$original_data$referrals_percent_change
      r$chart_specification$referrals_change_type <- r$chart_specification$original_data$referrals_change_type
      r$chart_specification$scenario_type <- r$chart_specification$original_data$scenario_type
      r$chart_specification$capacity_percent_change <- r$chart_specification$original_data$capacity_percent_change
      r$chart_specification$capacity_change_type <- r$chart_specification$original_data$capacity_change_type

      r$chart_specification$optimise_status <- NULL
    })

    # plot clicks -------------------------------------------------------------

    observeEvent(
      c(input$plot_click),
      {
        reactive_data$plot_clicked <- TRUE

        x_val <- as.Date(input$plot_click$x)
        reactive_data$plot_click_info <- click_info(
          data = reactive_data$plot_data,
          click_x = x_val,
          facet = input$plot_click$panelvar1
        )
      }
    )

    # Render the reneges value box based on click data
    output$value_box_container <- renderUI({
      if (isTRUE(reactive_data$plot_clicked)) {
        value_box_info <- dplyr::tribble(
          ~button                                ,
          ~title                                 ,
          ~y_title                               ,
          ~y_val_type                            ,
          "btn_referrals"                        ,
          "Referral count information"           ,
          "Referrals"                            ,
          "number"                               ,
          "btn_capacity_ttl"                     ,
          "Treatment capacity count information" ,
          "Treatment capacity"                   ,
          "number"                               ,
          "btn_capacity_mnth"                    ,
          "Treatment capacity count information" ,
          "Treatment capacity"                   ,
          "number"                               ,
          "btn_reneges_ttl"                      ,
          "Renege count information"             ,
          "Reneges"                              ,
          "number"                               ,
          "btn_reneges_mnth"                     ,
          "Renege count information"             ,
          "Reneges"                              ,
          "number"                               ,
          "btn_waiting_list_ttl"                 ,
          "Waiting list information"             ,
          "Waiting list size"                    ,
          "number"                               ,
          "btn_waiting_list_mnth"                ,
          "Waiting list information"             ,
          "Waiting list size"                    ,
          "number"                               ,
          "btn_performance"                      ,
          "Performance information"              ,
          "Performance"                          ,
          "percent"                              ,
          "btn_shortfall"                        ,
          "Shortfall information"                ,
          "Number of long waiters"               ,
          "number"
        ) |>
          dplyr::filter(
            .data$button == reactive_data$btn_val
          )

        layout_column_wrap(
          width = "400px",
          fixed_width = TRUE,
          bslib::value_box(
            title = value_box_info[["title"]],
            value = value_box_text(
              x_val = reactive_data$plot_click_info$period,
              y_title = value_box_info[["y_title"]],
              y_val = reactive_data$plot_click_info$p_var,
              y_val_type = value_box_info[["y_val_type"]],
              facet = reactive_data$plot_click_info$months_waited_id
            ),
            showcase = shiny::icon("chart-line"),
            theme = "purple",
            full_screen = FALSE,
            fill = TRUE,
            id = "performance"
          )
        )
      }
    })
  })
}

## To be copied in the UI
# mod_03_results_ui("03_results_1")

## To be copied in the server
# mod_03_results_server("03_results_1")

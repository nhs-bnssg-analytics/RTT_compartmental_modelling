#' 03_results UI Function
#'
#' @description Module that displays the results
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList clickOpts uiOutput p actionButton plotOutput clickOpts
#' @importFrom DT DTOutput
#' @importFrom bslib navset_tab nav_panel card card_body page_sidebar
#'   layout_column_wrap
#' @importFrom dplyr if_else
#' @importFrom tidyr pivot_wider
mod_03_results_ui <- function(id){
  ns <- NS(id)

  page_sidebar(
    sidebar = sidebar(
      actionButton(
        inputId = ns("btn_referrals"),
        label = "Referrals"
      ),
      p("Treatment capacity"),
      actionButton(
        inputId = ns("btn_capacity_ttl"),
        label = "Total"
      ),
      actionButton(
        inputId = ns("btn_capacity_mnth"),
        label = "Distribution"
      ),
      p("Reneges"),
      actionButton(
        inputId = ns("btn_reneges_ttl"),
        label = "Total"
      ),
      actionButton(
        inputId = ns("btn_reneges_mnth"),
        label = "Distribution"
      ),
      p("Waiting list size"),
      actionButton(
        inputId = ns("btn_waiting_list_ttl"),
        label = "Total"
      ),
      actionButton(
        inputId = ns("btn_waiting_list_mnth"),
        label = "Distribution"
      ),
      actionButton(
        inputId = ns("btn_performance"),
        label = "18 week performance"
      ),
      actionButton(
        inputId = ns("btn_data"),
        label = "Data table"
      ),
      uiOutput(ns("btn_report_ui"))
    ),
    card(
      card_body(
        plotOutput(
          ns("results_plot"),
          click = shiny::clickOpts(
            id = ns("plot_click")
          ),
          height = "600px"
        ),
        min_height = '60vh'
      )
    ),
    layout_column_wrap(
      width = 1/2,
      uiOutput(
        ns("value_box_container")
      )
    )
  )
}

#' 03_results Server Functions
#' @importFrom DT renderDT formatRound datatable
#' @importFrom dplyr group_by rename summarise tribble
#' @importFrom rlang .data
#' @importFrom bslib value_box
#' @import ggplot2
#' @noRd
mod_03_results_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    reactive_data <- reactiveValues()
    reactive_data$plot_click_info <- NULL
    reactive_data$plot_clicked <- NULL
    reactive_data$btn_val <- NULL
    reactive_data$plot_data <- NULL

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
            "Generate report"#,
            # style = "width:25%;"
          )
        }
      }
    })


    output$btn_report <- downloadHandler(
      filename <-  "Trust planning report.docx",
      content = function(file) {

        tempReport <- file.path(tempdir(), "skeleton.Rmd")

        file.copy(
          system.file("rmarkdown", "templates", "scenario-report", "skeleton", "skeleton.Rmd", package = "RTTshiny"),
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
          chart_specification = r$chart_specification
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
        "data",
        "report_ui"
      ),
      FUN = function(i) {
        observeEvent(input[[paste0("btn_", i)]], {
          if (input[[paste0("btn_", i)]] > 0) {
            reactive_data$btn_val <- paste0("btn_", i)
          }
          reactive_data$plot_clicked <- FALSE
        })
      }
    )

# plot --------------------------------------------------------------------

    output$results_plot <- renderPlot({

      if (is.null(r$waiting_list) | is.null(reactive_data$btn_val)) {
        holding_chart()
      } else {
        if (reactive_data$btn_val == "btn_referrals") {
          reactive_data$plot_data <- r$waiting_list |>
            dplyr::filter(.data$months_waited_id == "0-1 months") |>
            dplyr::mutate(p_var  = sum(.data$adjusted_referrals),
                          .by = c("period", "period_type")) |>
            extend_period_type_data()

          plot_output(data = reactive_data$plot_data,
                      p_trust = r$chart_specification$trust,
                      p_speciality = r$chart_specification$specialty,
                      p_chart = "referrals",
                      p_scenario = r$chart_specification$scenario_type,
                      p_cap_change = r$chart_specification$capacity_percent_change,
                      p_cap_skew = r$chart_specification$capacity_skew,
                      p_cap_change_type = r$chart_specification$capacity_change_type,
                      p_target_data = r$chart_specification$target_data,
                      p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                      p_referrals_change_type = r$chart_specification$referrals_change_type,
                      p_perc = F)
        } else if (reactive_data$btn_val == "btn_capacity_ttl") {
          reactive_data$plot_data <- r$waiting_list |>
            dplyr::summarise(p_var = sum(.data$calculated_treatments, na.rm = T),
                             .by = c("period", "period_type")) |>
            extend_period_type_data()


          plot_output(data = reactive_data$plot_data,
                      p_trust = r$chart_specification$trust,
                      p_speciality = r$chart_specification$specialty,
                      p_chart = "total treatment capacity",
                      p_scenario = r$chart_specification$scenario_type,
                      p_cap_change = r$chart_specification$capacity_percent_change,
                      p_cap_skew = r$chart_specification$capacity_skew,
                      p_cap_change_type = r$chart_specification$capacity_change_type,
                      p_target_data = r$chart_specification$target_data,
                      p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                      p_referrals_change_type = r$chart_specification$referrals_change_type,
                      p_perc = F,
                      p_facet = F)

        } else if (reactive_data$btn_val == "btn_capacity_mnth") {
          reactive_data$plot_data <- r$waiting_list |>
            dplyr::summarise(p_var = sum(.data$calculated_treatments, na.rm = T),
                             .by = c("period", "period_type", "months_waited_id")) |>
            extend_period_type_data()

          plot_output(data = reactive_data$plot_data,
                      p_trust = r$chart_specification$trust,
                      p_speciality = r$chart_specification$specialty,
                      p_chart = "treatment capacity by months waiting",
                      p_scenario = r$chart_specification$scenario_type,
                      p_cap_change = r$chart_specification$capacity_percent_change,
                      p_cap_skew = r$chart_specification$capacity_skew,
                      p_cap_change_type = r$chart_specification$capacity_change_type,
                      p_target_data = r$chart_specification$target_data,
                      p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                      p_referrals_change_type = r$chart_specification$referrals_change_type,
                      p_perc = F,
                      p_facet = T)
        } else if (reactive_data$btn_val == "btn_reneges_ttl") {
          reactive_data$plot_data <- r$waiting_list |>
            dplyr::summarise(p_var = sum(.data$reneges, na.rm = T),
                             .by = c("period", "period_type")) |>
            extend_period_type_data()

          plot_output(data = reactive_data$plot_data,
                      p_trust = r$chart_specification$trust,
                      p_speciality = r$chart_specification$specialty,
                      p_chart = "total net reneges",
                      p_scenario = r$chart_specification$scenario_type,
                      p_cap_change = r$chart_specification$capacity_percent_change,
                      p_cap_skew = r$chart_specification$capacity_skew,
                      p_cap_change_type = r$chart_specification$capacity_change_type,
                      p_target_data = r$chart_specification$target_data,
                      p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                      p_referrals_change_type = r$chart_specification$referrals_change_type,
                      p_perc = F)
        } else if (reactive_data$btn_val == "btn_reneges_mnth") {
          reactive_data$plot_data <- r$waiting_list |>
            dplyr::summarise(p_var = sum(.data$reneges, na.rm = T),
                             .by = c("period", "period_type", "months_waited_id")) |>
            extend_period_type_data()

          plot_output(data = reactive_data$plot_data,
                      p_trust = r$chart_specification$trust,
                      p_speciality = r$chart_specification$specialty,
                      p_chart = "net reneges by months waiting",
                      p_scenario = r$chart_specification$scenario_type,
                      p_cap_change = r$chart_specification$capacity_percent_change,
                      p_cap_skew = r$chart_specification$capacity_skew,
                      p_cap_change_type = r$chart_specification$capacity_change_type,
                      p_target_data = r$chart_specification$target_data,
                      p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                      p_referrals_change_type = r$chart_specification$referrals_change_type,
                      p_perc = F,
                      p_facet = T)
        } else if (reactive_data$btn_val == "btn_waiting_list_ttl") {
          reactive_data$plot_data <- r$waiting_list |>
            dplyr::summarise(p_var = sum(.data$incompletes, na.rm = T),
                             .by = c("period", "period_type")) |>
            extend_period_type_data()

          plot_output(data = reactive_data$plot_data,
                      p_trust = r$chart_specification$trust,
                      p_speciality = r$chart_specification$specialty,
                      p_chart = "waiting list size",
                      p_scenario = r$chart_specification$scenario_type,
                      p_cap_change = r$chart_specification$capacity_percent_change,
                      p_cap_skew = r$chart_specification$capacity_skew,
                      p_cap_change_type = r$chart_specification$capacity_change_type,
                      p_target_data = r$chart_specification$target_data,
                      p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                      p_referrals_change_type = r$chart_specification$referrals_change_type,
                      p_perc = F)
        } else if (reactive_data$btn_val == "btn_waiting_list_mnth") {
          reactive_data$plot_data <- r$waiting_list |>
            dplyr::mutate(p_var = .data$incompletes) |>
            extend_period_type_data()

          plot_output(data = reactive_data$plot_data,
                      p_trust = r$chart_specification$trust,
                      p_speciality = r$chart_specification$specialty,
                      p_chart = "numbers waiting by period",
                      p_scenario = r$chart_specification$scenario_type,
                      p_cap_change = r$chart_specification$capacity_percent_change,
                      p_cap_skew = r$chart_specification$capacity_skew,
                      p_cap_change_type = r$chart_specification$capacity_change_type,
                      p_target_data = r$chart_specification$target_data,
                      p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                      p_referrals_change_type = r$chart_specification$referrals_change_type,
                      p_perc = F,
                      p_facet = T)
        } else if (reactive_data$btn_val == "btn_performance") {
          reactive_data$plot_data <- r$waiting_list |>
            dplyr::rename(value = "incompletes") |>
            dplyr::group_by(.data$period_type) |>
            mutate(
              months_waited_id = extract_first_number(.data$months_waited_id)
            ) |>
            calc_performance(
              target_bin = 4
            ) |>
            ungroup() |>
            rename(p_var = "prop") |>
            extend_period_type_data()


          plot_output(data = reactive_data$plot_data,
                      p_trust = r$chart_specification$trust,
                      p_speciality = r$chart_specification$specialty,
                      p_chart = "18 week performance",
                      p_scenario = r$chart_specification$scenario_type,
                      p_cap_change = r$chart_specification$capacity_percent_change,
                      p_cap_skew = r$chart_specification$capacity_skew,
                      p_cap_change_type = r$chart_specification$capacity_change_type,
                      p_target_data = r$chart_specification$target_data,
                      p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                      p_referrals_change_type = r$chart_specification$referrals_change_type,
                      p_perc = T,
                      p_target_line = T)
        }

      }

    }, res = 96)


# plot clicks -------------------------------------------------------------

    observeEvent(
      c(input$plot_click), {

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
        # browser()
        value_box_info <- dplyr::tribble(
                          ~button,                                 ~title,             ~y_title, ~y_val_type,
                  "btn_referrals",           "Referral count information",          "Referrals",    "number",
               "btn_capacity_ttl", "Treatment capacity count information", "Treatment capacity",    "number",
              "btn_capacity_mnth", "Treatment capacity count information", "Treatment capacity",    "number",
                "btn_reneges_ttl",             "Renege count information",            "Reneges",    "number",
               "btn_reneges_mnth",             "Renege count information",            "Reneges",    "number",
           "btn_waiting_list_ttl",             "Waiting list information",  "Waiting list size",    "number",
          "btn_waiting_list_mnth",             "Waiting list information",  "Waiting list size",    "number",
                "btn_performance",              "Performance information",        "Performance",   "percent"
          ) |>
          dplyr::filter(
            .data$button == reactive_data$btn_val
          )


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
          full_screen = TRUE,
          fill = TRUE,
          id = "performance"
        )
      }
    })
# DT table ----------------------------------------------------------------


      output$scenario_projections <- DT::renderDT({

        new_col_names <- c(
          "Period ID" = "period_id",
          "Months waited (lower)" = "months_waited_id",
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
            period_id = NA,
            months_waited_id = NA,
            calculated_treatments = NA,
            reneges = NA,
            incompletes = NA,
            unadjusted_referrals = NA,
            adjusted_referrals = NA,
            capacity_skew = NA,
            period_type = NA,
            period = NA
          ) |>
            DT::datatable(
              colnames = new_col_names,
              rownames = FALSE,
              caption = "Please return to the Scenario tab to create some modelled data",
              options = list(
                dom = 't', # 't' means show only the table, no other elements
                paging = FALSE, # Disable pagination
                searching = FALSE, # Disable search box
                info = FALSE # Remove "Showing X of Y entries" text
              )
            )
        } else {


          DT::datatable(
            r$waiting_list,
            filter = "top",
            extensions = "Buttons",
            options = list(
              paging = TRUE,
              pageLength = 50,
              lengthMenu = c(25, 50, 100),
              searching = TRUE,
              ordering = TRUE,
              autoWidth = TRUE,
              dom = 'Bfrtip',
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
      server = FALSE)
  })
}

## To be copied in the UI
# mod_03_results_ui("03_results_1")

## To be copied in the server
# mod_03_results_server("03_results_1")

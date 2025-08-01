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
mod_03_results_ui <- function(id){
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
#' @importFrom DT renderDT formatRound datatable
#' @importFrom dplyr group_by rename summarise tribble
#' @importFrom rlang .data
#' @importFrom bslib value_box layout_column_wrap
#' @importFrom shiny updateActionButton showModal modalDialog downloadHandler
#'   actionButton p hr uiOutput
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
    reactive_data$show_plot <- FALSE
    reactive_data$show_table <- FALSE



# dynamic sidebar ---------------------------------------------------------

    output$dynamic_sidebar_ui <- renderUI({

      if (is.null(r$chart_specification$scenario_type)) {
        tagList()
      } else if (r$chart_specification$scenario_type == "Estimate performance (from treatment capacity inputs)") {

        layout_column_wrap(
          width = 1,
          gap = 0,
          p("INPUTS",
            class = "sidebar_header"),
          actionButton(
            inputId = ns("btn_referrals"),
            label = "Referrals",
            title = "Referral counts over the period",
            `data-bs-trigger` = "hover",
            icon = shiny::icon("star"),
            class = "results_button"
          ),
          p("Treatment capacity",
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
          p("OUTPUTS",
            class = "sidebar_header"),
          p("Waiting list size",
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
          p("Reneges",
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


      } else if (r$chart_specification$scenario_type == "Estimate treatment capacity (from performance targets)") {

        layout_column_wrap(
          width = 1,
          gap = 0,
          p("INPUTS",
            class = "sidebar_header"),
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
          p("OUTPUTS",
            class = "sidebar_header"),
          p("Treatment capacity",
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
          p("Reneges",
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
          p("Waiting list size",
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
            "Generate report"#,
            # style = "width:25%;"
          )
        }
      }
    })


    output$btn_report <- downloadHandler(
      filename <-  paste0(
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
              # icon = shiny::icon("calendar")
            )
          }
          reactive_data$plot_clicked <- FALSE

          if (reactive_data$btn_val == "btn_data") {
            reactive_data$show_plot <- FALSE
            reactive_data$show_table <- TRUE
          } else if (reactive_data$btn_val != "btn_report_ui") {
            reactive_data$show_plot <- TRUE
            reactive_data$show_table <- FALSE
          }
        })
      }
    )

# DT table ----------------------------------------------------------------


    output$results_table <- DT::renderDT({

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
            "period", .before = dplyr::everything()
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
    server = FALSE)

# plot --------------------------------------------------------------------

    observeEvent(
      c(input$chart_res,
        input$btn_referrals,
        input$btn_capacity_ttl,
        input$btn_capacity_mnth,
        input$btn_reneges_ttl,
        input$btn_reneges_mnth,
        input$btn_waiting_list_ttl,
        input$btn_waiting_list_mnth,
        input$btn_performance,
        input$btn_data,
        input$btn_report_ui
      ), {
        if (is.null(input$chart_res)) {
          calc_res <- 96
        } else {
          calc_res <- input$chart_res
        }
        output$results_plot <- renderPlot({

          # browser()
          if (is.null(r$waiting_list) | identical(r$waiting_list, tibble())) {
            holding_chart(type = "model")
          } else if (is.null(reactive_data$btn_val)) {
            holding_chart(type = "select_chart")
          } else {
            if (reactive_data$btn_val == "btn_referrals") {
              reactive_data$plot_data <- r$waiting_list |>
                dplyr::filter(.data$months_waited_id == "0-1 months") |>
                dplyr::mutate(p_var  = sum(.data$adjusted_referrals),
                              .by = c("period", "period_type")) |>
                extend_period_type_data()

              chart_type <- "referrals"
              include_facets <- FALSE
              percentage_axis <- FALSE
              include_target_line <- FALSE

            } else if (reactive_data$btn_val == "btn_capacity_ttl") {
              reactive_data$plot_data <- r$waiting_list |>
                dplyr::summarise(p_var = sum(.data$calculated_treatments, na.rm = T),
                                 .by = c("period", "period_type")) |>
                extend_period_type_data()

              chart_type <- "total treatment capacity"
              include_facets <- FALSE
              percentage_axis <- FALSE
              include_target_line <- FALSE

            } else if (reactive_data$btn_val == "btn_capacity_mnth") {
              reactive_data$plot_data <- r$waiting_list |>
                dplyr::summarise(p_var = sum(.data$calculated_treatments, na.rm = T),
                                 .by = c("period", "period_type", "months_waited_id")) |>
                extend_period_type_data()

              chart_type <- "treatment capacity by months waiting"
              include_facets <- TRUE
              percentage_axis <- FALSE
              include_target_line <- FALSE

            } else if (reactive_data$btn_val == "btn_reneges_ttl") {
              reactive_data$plot_data <- r$waiting_list |>
                dplyr::summarise(p_var = sum(.data$reneges, na.rm = T),
                                 .by = c("period", "period_type")) |>
                extend_period_type_data()

              chart_type <- "total net reneges"
              include_facets <- FALSE
              percentage_axis <- FALSE
              include_target_line <- FALSE

            } else if (reactive_data$btn_val == "btn_reneges_mnth") {
              reactive_data$plot_data <- r$waiting_list |>
                dplyr::summarise(p_var = sum(.data$reneges, na.rm = T),
                                 .by = c("period", "period_type", "months_waited_id")) |>
                extend_period_type_data()

              chart_type <- "net reneges by months waiting"
              include_facets <- TRUE
              percentage_axis <- FALSE
              include_target_line <- FALSE

            } else if (reactive_data$btn_val == "btn_waiting_list_ttl") {
              reactive_data$plot_data <- r$waiting_list |>
                dplyr::summarise(p_var = sum(.data$incompletes, na.rm = T),
                                 .by = c("period", "period_type")) |>
                extend_period_type_data()

              chart_type <- "waiting list size"
              include_facets <- FALSE
              percentage_axis <- FALSE
              include_target_line <- FALSE

            } else if (reactive_data$btn_val == "btn_waiting_list_mnth") {
              reactive_data$plot_data <- r$waiting_list |>
                dplyr::mutate(p_var = .data$incompletes) |>
                extend_period_type_data()

              chart_type <- "numbers waiting by period"
              include_facets <- TRUE
              percentage_axis <- FALSE
              include_target_line <- FALSE

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


              chart_type <- "18 weeks performance"
              include_facets <- FALSE
              percentage_axis <- TRUE
              include_target_line <- TRUE

            }

            if (!(reactive_data$btn_val %in% c("btn_data", "btn_report_ui"))) {
              plot_output(data = reactive_data$plot_data,
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
                          p_target_line = include_target_line)
            }
          }

        }, res = calc_res)
      }
    )





# dynamic ui --------------------------------------------------------------
    output$results_ui <- renderUI({
       if (reactive_data$show_table ==  TRUE) {
        DTOutput(
          ns("results_table")
        )
      } else {
        div(
          plotOutput(
            ns("results_plot"),
            click = shiny::clickOpts(
              id = ns("plot_click")
            ),
            height = "600px"
          ),
          div(
            class = "label-left",
            sliderInput(
              inputId = ns("chart_res"),
              label = "Select chart resolution (pixels per inch)",
              min = 72,
              max = 144,
              value = 96,
              step = 8
            )
          )
        )
      }
    })


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

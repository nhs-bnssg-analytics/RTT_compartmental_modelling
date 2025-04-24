#' 03_results UI Function
#'
#' @description Module that displays the results
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList clickOpts
#' @importFrom DT DTOutput
#' @importFrom bslib navset_tab nav_panel card card_body
#' @importFrom dplyr if_else
#' @importFrom tidyr pivot_wider
mod_03_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    navset_tab(
      nav_panel(
        title = "Table",
        card(
          card_body(
            DT::DTOutput(
              ns("scenario_projections")
            ),
            min_height = '80vh'
          )
        )
      ),
      nav_panel(
        title = "Total waiting list size",
        p(""),
        card(
          card_body(
            plotOutput(
              ns("wl_size"),
              click = shiny::clickOpts(
                id = ns("wl_plot_click")
              ),
              height = "600px"
            ),
            uiOutput(
              ns("value_box_container_wl")
            ),
            min_height = '60vh'
          )
        )
      ),
      nav_panel(
        title = "Waiting list size by months waiting",
        p(""),
        card(
          card_body(
            plotOutput(
          ns("wl_wait_per"),
          click = shiny::clickOpts(
            id = ns("wl_split_plot_click")
          ),
          height = "600px"
        ),
        uiOutput(
          ns("value_box_container_wl_split")
        ),
        min_height = '60vh')
        )
      ),
      nav_panel(
        title = "18 week performance",
        p(""),
        card(
          card_body(
            plotOutput(
              ns("wl_performance"),
              click = shiny::clickOpts(
                id = ns("performance_plot_click")
              ),
              height = "600px"
            ),
            uiOutput(
              ns("value_box_container_perf")
            ),
            min_height = '60vh'
          )
        )
      ),
      nav_panel(
        title = "Referrals",
        p(""),
        card(
          card_body(
            plotOutput(
              ns("wl_referrals"),
              click = shiny::clickOpts(
                id = ns("referrals_plot_click")
              ),
              height = "600px"
            ),
            uiOutput(
              ns("value_box_container_ref")
            ),
            min_height = '60vh'
          )
        )
      ),
      nav_panel(
        title = "Net total reneges",
        p(""),
        card(
          card_body(
            plotOutput(
              ns("wl_reneging_plot_total"),
              click = shiny::clickOpts(
                id = ns("reneges_plot_click")
              ),
              height = "600px"
            ),
            uiOutput(
              ns("value_box_container_ren")
            ),
            min_height = '60vh'
          )
        )
      ),
      nav_panel(
        title = "Number of reneges by month waiting",
        p(""),
        card(
          card_body(
            plotOutput(
              ns("wl_reneging_plot_split"),
              click = shiny::clickOpts(
                id = ns("reneges_split_plot_click")
              ),
              height = "600px"
            ),
            uiOutput(
              ns("value_box_container_ren_split")
            ),
            min_height = '60vh'
          )
        )
      ),
      nav_panel(
        title = "Total treatment capacity",
        p(""),
        card(
          card_body(
            plotOutput(
              ns("wl_capacity_tot"),
              click = shiny::clickOpts(
                id = ns("capacity_plot_click")
              ),
              height = "600px"
            ),
            uiOutput(
              ns("value_box_container_cap")
            ),
            min_height = '60vh'
          )
        )
      ),
      nav_panel(
        title = "Treatment capacity split by months waiting",
        p(""),
        card(
          card_body(
            plotOutput(
              ns("wl_capacity_split"),
              click = shiny::clickOpts(
                id = ns("capacity_split_plot_click")
              ),
              height = "600px"
            ),
            uiOutput(
              ns("value_box_container_cap_split")
            ),
            min_height = '60vh'
          )
        )
      )
    )
  )
}

#' 03_results Server Functions
#' @importFrom DT renderDT formatRound datatable
#' @importFrom dplyr group_by rename summarise
#' @importFrom rlang .data
#' @importFrom bslib value_box
#' @import ggplot2
#' @noRd
mod_03_results_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    reactive_clicks <- reactiveValues()
    reactive_clicks$wl_plot_click <- NULL
    reactive_clicks$performance_plot_click <- NULL
    reactive_clicks$referrals_plot_click <- NULL
    reactive_clicks$reneges_plot_click <- NULL
    reactive_clicks$capacity_plot_click <- NULL
    reactive_clicks$wl_split_plot_click <- NULL
    reactive_clicks$reneges_split_plot_click <- NULL
    reactive_clicks$capacity_split_plot_click <- NULL

    reactive_datasets <- reactiveValues()

    # waiting list size
    reactive_datasets$dat_size <- NULL
    reactive_datasets$dat_size_clicked <- NULL
    # waiting list split size
    reactive_datasets$dat_size_split <- NULL
    reactive_datasets$dat_size_split_clicked <- NULL
    # performance
    reactive_datasets$dat_perf <- NULL
    reactive_datasets$dat_perf_clicked <- NULL
    # referrals
    reactive_datasets$dat_ref <- NULL
    reactive_datasets$dat_ref_clicked <- NULL
    # reneges
    reactive_datasets$dat_ren <- NULL
    reactive_datasets$dat_ren_clicked <- NULL
    # reneges
    reactive_datasets$dat_ren_split <- NULL
    reactive_datasets$dat_ren_split_clicked <- NULL
    # treatment capacity
    reactive_datasets$dat_cap <- NULL
    reactive_datasets$dat_cap_clicked <- NULL
    # treatment capacity
    reactive_datasets$dat_cap_split <- NULL
    reactive_datasets$dat_cap_split_clicked <- NULL



    # when WL plot is clicked -------------------------------------------------

    observeEvent(
      c(input$wl_plot_click), {
        reactive_clicks$wl_plot_click <- TRUE

        x_val <- as.Date(input$wl_plot_click$x)
        reactive_datasets$dat_size_clicked <- click_info(
          data = reactive_datasets$dat_size,
          click_x = x_val
        )

      }
    )
    # Render the waiting list value box based on click data
    output$value_box_container_wl <- renderUI({
      if (isTRUE(reactive_clicks$wl_plot_click)) {

        bslib::value_box(
          title = "Waiting list information",
          value = value_box_text(
            x_val = reactive_datasets$dat_size_clicked$period,
            y_title = "Waiting list size",
            y_val = reactive_datasets$dat_size_clicked$p_var,
            y_val_type = "number"
          ),
          showcase = shiny::icon("chart-line"),
          theme = "purple",
          full_screen = TRUE,
          fill = TRUE
        )
      }
    })

    ## Create waiting list size plot(s)
    output$wl_size <- renderPlot({
      reactive_datasets$dat_size <- r$waiting_list |>
        dplyr::summarise(p_var = sum(.data$incompletes, na.rm = T),
                         .by = c("period", "period_type")) |>
        extend_period_type_data()

      plot_output(data = reactive_datasets$dat_size,
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

    }, res = 96)



    # When waiting list size split plot is clicked --------------------------------
    observeEvent(
      c(input$wl_split_plot_click), {
        reactive_clicks$wl_split_plot_click <- TRUE

        x_val <- as.Date(input$wl_split_plot_click$x)
        reactive_datasets$dat_size_split_clicked <- click_info(
          data = reactive_datasets$dat_size_split,
          click_x = x_val,
          facet = input$wl_split_plot_click$panelvar1
        )
      }
    )

    # Render the waiting list value box based on click data
    output$value_box_container_wl_split <- renderUI({
      if (isTRUE(reactive_clicks$wl_split_plot_click)) {

        bslib::value_box(
          title = "Waiting list information",
          value = value_box_text(
            x_val = reactive_datasets$dat_size_split_clicked$period,
            y_title = "Waiting list size",
            y_val = reactive_datasets$dat_size_split_clicked$p_var,
            y_val_type = "number",
            facet = reactive_datasets$dat_size_split_clicked$months_waited_id
          ),
          showcase = shiny::icon("chart-line"),
          theme = "purple",
          full_screen = TRUE,
          fill = TRUE
        )
      }
    })

    ## Create waiting by split
    output$wl_wait_per <- renderPlot({

      reactive_datasets$dat_size_split <- r$waiting_list |>
        dplyr::mutate(p_var = .data$incompletes) |>
        extend_period_type_data()

      plot_output(data = reactive_datasets$dat_size_split,
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

    }, res = 96)

    # when performance plot is clicked ----------------------------------------

    observeEvent(
      c(input$performance_plot_click), {
        reactive_clicks$performance_plot_click <- TRUE

        x_val <- as.Date(input$performance_plot_click$x)
        reactive_datasets$dat_perf_clicked <- click_info(
          data = reactive_datasets$dat_perf,
          click_x = x_val
        )
      }
    )

    # Render the performance value box based on click data
    output$value_box_container_perf <- renderUI({

      if (isTRUE(reactive_clicks$performance_plot_click)) {

        bslib::value_box(
          title = "Performance information",
          value = value_box_text(
            x_val = reactive_datasets$dat_perf_clicked$period,
            y_title = "Performance",
            y_val = reactive_datasets$dat_perf_clicked$p_var,
            y_val_type = "percent"
          ),
          showcase = shiny::icon("chart-line"),
          theme = "purple",
          full_screen = TRUE,
          fill = TRUE
        )
      }
    })

    ## Create waiting 18 week performance plots here
    output$wl_performance <- renderPlot({

      reactive_datasets$dat_perf <- r$waiting_list |>
        dplyr::rename(value = "incompletes") |>
        dplyr::group_by(.data$period_type) |>
        calc_performance(
          target_bin = 4
        ) |>
        ungroup() |>
        rename(p_var = "prop") |>
        extend_period_type_data()


      plot_output(data = reactive_datasets$dat_perf,
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

    }, res = 96)


# When referrals plot is clicked ------------------------------------------
    observeEvent(
      c(input$referrals_plot_click), {
        reactive_clicks$referrals_plot_click <- TRUE

        x_val <- as.Date(input$referrals_plot_click$x)
        reactive_datasets$dat_ref_clicked <- click_info(
          data = reactive_datasets$dat_ref,
          click_x = x_val
        )
      }
    )

    # Render the referrals value box based on click data
    output$value_box_container_ref <- renderUI({

      if (isTRUE(reactive_clicks$referrals_plot_click)) {

        bslib::value_box(
          title = "Referral count information",
          value = value_box_text(
            x_val = reactive_datasets$dat_ref_clicked$period,
            y_title = "Referrals",
            y_val = reactive_datasets$dat_ref_clicked$p_var,
            y_val_type = "number"
          ),
          showcase = shiny::icon("chart-line"),
          theme = "purple",
          full_screen = TRUE,
          fill = TRUE
        )
      }
    })

    ## Create referrals plots
    output$wl_referrals <- renderPlot({

      reactive_datasets$dat_ref <- r$waiting_list |>
        dplyr::filter(.data$months_waited_id == 0) |>
        dplyr::mutate(p_var  = sum(.data$adjusted_referrals),
                      .by = c("period", "period_type")) |>
        extend_period_type_data()

      plot_output(data = reactive_datasets$dat_ref,
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

    }, res = 96)

# When reneges plot is clicked ------------------------------------------
    observeEvent(
      c(input$reneges_plot_click), {
        reactive_clicks$reneges_plot_click <- TRUE

        x_val <- as.Date(input$reneges_plot_click$x)
        reactive_datasets$dat_ren_clicked <- click_info(
          data = reactive_datasets$dat_ren,
          click_x = x_val
        )
      }
    )

    # Render the reneges value box based on click data
    output$value_box_container_ren <- renderUI({

      if (isTRUE(reactive_clicks$reneges_plot_click)) {

        bslib::value_box(
          title = "Reneges count information",
          value = value_box_text(
            x_val = reactive_datasets$dat_ren_clicked$period,
            y_title = "Reneges",
            y_val = reactive_datasets$dat_ren_clicked$p_var,
            y_val_type = "number"
          ),
          showcase = shiny::icon("chart-line"),
          theme = "purple",
          full_screen = TRUE,
          fill = TRUE
        )
      }
    })

    ## Create reneging plots - total
    output$wl_reneging_plot_total <- renderPlot({

      reactive_datasets$dat_ren <- r$waiting_list |>
        dplyr::summarise(p_var = sum(.data$reneges, na.rm = T),
                         .by = c("period", "period_type")) |>
        extend_period_type_data()

      plot_output(data = reactive_datasets$dat_ren,
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


    }, res = 96)

# When reneges split plot is clicked ------------------------------------------
    observeEvent(
      c(input$reneges_split_plot_click), {
        reactive_clicks$reneges_split_plot_click <- TRUE

        x_val <- as.Date(input$reneges_split_plot_click$x)
        reactive_datasets$dat_ren_split_clicked <- click_info(
          data = reactive_datasets$dat_ren_split,
          click_x = x_val,
          facet = input$reneges_split_plot_click$panelvar1
        )
      }
    )

    # Render the reneges value box based on click data
    output$value_box_container_ren_split <- renderUI({

      if (isTRUE(reactive_clicks$reneges_split_plot_click)) {

        bslib::value_box(
          title = "Reneges count information",
          value = value_box_text(
            x_val = reactive_datasets$dat_ren_split_clicked$period,
            y_title = "Reneges",
            y_val = reactive_datasets$dat_ren_split_clicked$p_var,
            y_val_type = "number",
            facet = reactive_datasets$dat_ren_split_clicked$months_waited_id
          ),
          showcase = shiny::icon("chart-line"),
          theme = "purple",
          full_screen = TRUE,
          fill = TRUE
        )
      }
    })

    ## Create reneging plots - split
    output$wl_reneging_plot_split <- renderPlot({

      reactive_datasets$dat_ren_split <- r$waiting_list |>
        dplyr::summarise(p_var = sum(.data$reneges, na.rm = T),
                         .by = c("period", "period_type", "months_waited_id")) |>
        extend_period_type_data()

      plot_output(data = reactive_datasets$dat_ren_split,
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

    }, res = 96)

    # When treatment capacity plot is clicked ------------------------------------------
    observeEvent(
      c(input$capacity_plot_click), {
        reactive_clicks$capacity_plot_click <- TRUE

        x_val <- as.Date(input$capacity_plot_click$x)
        reactive_datasets$dat_cap_clicked <- click_info(
          data = reactive_datasets$dat_cap,
          click_x = x_val
        )
      }
    )

    # Render the reneges value box based on click data
    output$value_box_container_cap <- renderUI({

      if (isTRUE(reactive_clicks$capacity_plot_click)) {

        bslib::value_box(
          title = "Treatment capacity count information",
          value = value_box_text(
            x_val = reactive_datasets$dat_cap_clicked$period,
            y_title = "Treatment capacity",
            y_val = reactive_datasets$dat_cap_clicked$p_var,
            y_val_type = "number"
          ),
          showcase = shiny::icon("chart-line"),
          theme = "purple",
          full_screen = TRUE,
          fill = TRUE
        )
      }
    })

    ## Create waiting list treatment capacity plot(s)
    output$wl_capacity_tot <- renderPlot({
      reactive_datasets$dat_cap <- r$waiting_list |>
        dplyr::summarise(p_var = sum(.data$calculated_treatments, na.rm = T),
                         .by = c("period", "period_type")) |>
        extend_period_type_data()


      plot_output(data = reactive_datasets$dat_cap,
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

    }, res = 96)

    # When treatment capacity plot is clicked ------------------------------------------
    observeEvent(
      c(input$capacity_split_plot_click), {
        reactive_clicks$capacity_split_plot_click <- TRUE

        x_val <- as.Date(input$capacity_split_plot_click$x)
        reactive_datasets$dat_cap_split_clicked <- click_info(
          data = reactive_datasets$dat_cap_split,
          click_x = x_val,
          facet = input$capacity_split_plot_click$panelvar1
        )
      }
    )

    # Render the reneges value box based on click data
    output$value_box_container_cap_split <- renderUI({

      if (isTRUE(reactive_clicks$capacity_split_plot_click)) {

        bslib::value_box(
          title = "Treatment capacity count information",
          value = value_box_text(
            x_val = reactive_datasets$dat_cap_split_clicked$period,
            y_title = "Treatment capacity",
            y_val = reactive_datasets$dat_cap_split_clicked$p_var,
            y_val_type = "number",
            facet = reactive_datasets$dat_cap_split_clicked$months_waited_id
          ),
          showcase = shiny::icon("chart-line"),
          theme = "purple",
          full_screen = TRUE,
          fill = TRUE
        )
      }
    })

    ## Create waiting list split treatment capacity plot(s)
    output$wl_capacity_split <- renderPlot({
      reactive_datasets$dat_cap_split<- r$waiting_list |>
        dplyr::summarise(p_var = sum(.data$calculated_treatments, na.rm = T),
                         .by = c("period", "period_type", "months_waited_id")) |>
        extend_period_type_data()

      plot_output(data = reactive_datasets$dat_cap_split,
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

    }, res = 96)



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

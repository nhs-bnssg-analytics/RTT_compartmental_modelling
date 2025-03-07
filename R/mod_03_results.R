#' 03_results UI Function
#'
#' @description Module that displays the results
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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
            )
          )
        )
      ),
      nav_panel(
        title = "WL Size",
        p("Second tab content."),
         plotOutput(
           ns("wl_size"),
           click = "plot_click"
         )
      ),
      nav_panel(
        title = "WL 4mth Performance",
        p("Second tab content."),
        plotOutput(
          ns("wl_performance"),
          click = NULL
                )
        ),
      nav_panel(
        title = "patients waiting across periods",
        p("Second tab content."),
        plotOutput(
          ns("wl_wait_per"),
          click = NULL
        )
      ),
      nav_panel(
        title = "WL Referrals",
        p("Second tab content."),
        plotOutput(
          ns("wl_referrals"),
          click = NULL
        )
      ),
      nav_panel(
        title = "WL Reneges Total",
        p("Second tab content."),
        plotOutput(
          ns("wl_reneging_plot_total"),
          click = NULL
        )
      ),
      nav_panel(
        title = "WL Reneges Split",
        p("Second tab content."),
        plotOutput(
          ns("wl_reneging_plot_split"),
          click = NULL
        )
      ),
      nav_panel(title = "Four", p("Second tab content.")),
      nav_panel(
        title = "WL Capacity",
        p("Second tab content."),
        plotOutput(
          ns("wl_capacity_tot"),
          click = NULL
        )
      ),
      nav_panel(
        title = "WL Capacity split",
        p("Second tab content."),
        plotOutput(
          ns("wl_capacity_split"),
          click = NULL
        )
      ),
      nav_panel(title = "Five", p("Second tab content."))
    )
  )
}

#' 03_results Server Functions
#' @importFrom DT renderDT formatRound datatable
#' @import ggplot2
#' @noRd
mod_03_results_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$scenario_projections <- DT::renderDT({

      DT::datatable(
        r$waiting_list,
        filter = "top"
      )
    })

    ## Create waiting list size plot(s)
    output$wl_size <- renderPlot({
      dat_size<- r$waiting_list |>
        dplyr::summarise(p_var = sum(incompletes, na.rm = T),
                         .by = c(period, period_type))

      plot_output(data = dat_size,
                  p_trust = r$chart_specification$trust,
                  p_speciality = r$chart_specification$specialty,
                  p_chart = "waiting list size",
                  p_scenario = r$chart_specification$scenario_type,
                  p_cap_change = r$chart_specification$capacity_percent_change,
                  p_cap_skew = r$chart_specification$capacity_skew,
                  p_cap_change_type = r$chart_specification$capacity_change_type,
                  p_target_date = r$chart_specification$target_date,
                  p_target_performance = r$chart_specification$target_performance,
                  p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                  p_referrals_change_type = r$chart_specification$referrals_change_type,
                  p_perc = F)

   }, res = 96)

    ## Create waiting 4 month performance plots here
    output$wl_performance <- renderPlot({
      dat_perf <- r$waiting_list |>
          dplyr::mutate(target_flag = dplyr::if_else (months_waited_id >= 4, 1, 0)) |>
          dplyr::summarise(tot_wait = sum(incompletes),
                           .by = c(target_flag, period, period_type)) |>
          dplyr::mutate(p_var = tot_wait / sum(tot_wait),
                        .by = c(period, period_type)) |>
          dplyr::filter(target_flag == 1)

      plot_output(data = dat_perf,
                  p_trust = r$chart_specification$trust,
                  p_speciality = r$chart_specification$specialty,
                  p_chart = "4 month performance",
                  p_scenario = r$chart_specification$scenario_type,
                  p_cap_change = r$chart_specification$capacity_percent_change,
                  p_cap_skew = r$chart_specification$capacity_skew,
                  p_cap_change_type = r$chart_specification$capacity_change_type,
                  p_target_date = r$chart_specification$target_date,
                  p_target_performance = r$chart_specification$target_performance,
                  p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                  p_referrals_change_type = r$chart_specification$referrals_change_type,
                  p_perc = T,
                  p_target_line = T)

    }, res = 96)

    ## Create waiting by bin
    output$wl_wait_per <- renderPlot({

      dat_wl_bin <- r$waiting_list |>
        dplyr::mutate(p_var = incompletes)

      plot_output(data = dat_wl_bin,
                  p_trust = r$chart_specification$trust,
                  p_speciality = r$chart_specification$specialty,
                  p_chart = "numbers waiting by period",
                  p_scenario = r$chart_specification$scenario_type,
                  p_cap_change = r$chart_specification$capacity_percent_change,
                  p_cap_skew = r$chart_specification$capacity_skew,
                  p_cap_change_type = r$chart_specification$capacity_change_type,
                  p_target_date = r$chart_specification$target_date,
                  p_target_performance = r$chart_specification$target_performance,
                  p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                  p_referrals_change_type = r$chart_specification$referrals_change_type,
                  p_perc = F,
                  p_facet = T)

    }, res = 96)



    ## Create waiting list referrals plots
    output$wl_referrals <- renderPlot({

      dat_ref <- r$waiting_list |>
        dplyr::filter(months_waited_id == 0) |>
        dplyr::mutate(p_var  = sum(incompletes + calculated_treatments),
                      .by = c(period, period_type))

      plot_output(data = dat_ref,
                  p_trust = r$chart_specification$trust,
                  p_speciality = r$chart_specification$specialty,
                  p_chart = "referrals",
                  p_scenario = r$chart_specification$scenario_type,
                  p_cap_change = r$chart_specification$capacity_percent_change,
                  p_cap_skew = r$chart_specification$capacity_skew,
                  p_cap_change_type = r$chart_specification$capacity_change_type,
                  p_target_date = r$chart_specification$target_date,
                  p_target_performance = r$chart_specification$target_performance,
                  p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                  p_referrals_change_type = r$chart_specification$referrals_change_type,
                  p_perc = F)

    }, res = 96)


    ## Create reneging plots - total
    output$wl_reneging_plot_total <- renderPlot({

      dat_ren <- r$waiting_list |>
        dplyr::summarise(p_var = sum(reneges, na.rm = T),
                         .by = c(period, period_type))

      plot_output(data = dat_ren,
                  p_trust = r$chart_specification$trust,
                  p_speciality = r$chart_specification$specialty,
                  p_chart = "total net reneges",
                  p_scenario = r$chart_specification$scenario_type,
                  p_cap_change = r$chart_specification$capacity_percent_change,
                  p_cap_skew = r$chart_specification$capacity_skew,
                  p_cap_change_type = r$chart_specification$capacity_change_type,
                  p_target_date = r$chart_specification$target_date,
                  p_target_performance = r$chart_specification$target_performance,
                  p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                  p_referrals_change_type = r$chart_specification$referrals_change_type,
                  p_perc = F)


    }, res = 96)

    ## Create reneging plots - split
    output$wl_reneging_plot_split <- renderPlot({

      dat_ren_split <- r$waiting_list |>
        dplyr::summarise(p_var = sum(reneges, na.rm = T),
                         .by = c(period, period_type, months_waited_id))

      plot_output(data = dat_ren_split,
                  p_trust = r$chart_specification$trust,
                  p_speciality = r$chart_specification$specialty,
                  p_chart = "net reneges by months waiting",
                  p_scenario = r$chart_specification$scenario_type,
                  p_cap_change = r$chart_specification$capacity_percent_change,
                  p_cap_skew = r$chart_specification$capacity_skew,
                  p_cap_change_type = r$chart_specification$capacity_change_type,
                  p_target_date = r$chart_specification$target_date,
                  p_target_performance = r$chart_specification$target_performance,
                  p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                  p_referrals_change_type = r$chart_specification$referrals_change_type,
                  p_perc = F,
                  p_facet = T)

    }, res = 96)

    ## Create waiting list capacity plot(s)
    output$wl_capacity_tot <- renderPlot({
      dat_cap_tot<- r$waiting_list |>
        dplyr::summarise(p_var = sum(calculated_treatments, na.rm = T),
                         .by = c(period, period_type))


      plot_output(data = dat_cap_tot,
                  p_trust = r$chart_specification$trust,
                  p_speciality = r$chart_specification$specialty,
                  p_chart = "total capacity",
                  p_scenario = r$chart_specification$scenario_type,
                  p_cap_change = r$chart_specification$capacity_percent_change,
                  p_cap_skew = r$chart_specification$capacity_skew,
                  p_cap_change_type = r$chart_specification$capacity_change_type,
                  p_target_date = r$chart_specification$target_date,
                  p_target_performance = r$chart_specification$target_performance,
                  p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                  p_referrals_change_type = r$chart_specification$referrals_change_type,
                  p_perc = F,
                  p_facet = F)

    }, res = 96)

    ## Create waiting list split capacity plot(s)
    output$wl_capacity_split <- renderPlot({
      dat_cap_tot<- r$waiting_list |>
        dplyr::summarise(p_var = sum(calculated_treatments, na.rm = T),
                         .by = c(period, period_type, months_waited_id))

      plot_output(data = dat_cap_tot,
                  p_trust = r$chart_specification$trust,
                  p_speciality = r$chart_specification$specialty,
                  p_chart = "capacity by months waiting",
                  p_scenario = r$chart_specification$scenario_type,
                  p_cap_change = r$chart_specification$capacity_percent_change,
                  p_cap_skew = r$chart_specification$capacity_skew,
                  p_cap_change_type = r$chart_specification$capacity_change_type,
                  p_target_date = r$chart_specification$target_date,
                  p_target_performance = r$chart_specification$target_performance,
                  p_referrals_percent_change = r$chart_specification$referrals_percent_change,
                  p_referrals_change_type = r$chart_specification$referrals_change_type,
                  p_perc = F,
                  p_facet = T)

    }, res = 96)

  })
}

## To be copied in the UI
# mod_03_results_ui("03_results_1")

## To be copied in the server
# mod_03_results_server("03_results_1")

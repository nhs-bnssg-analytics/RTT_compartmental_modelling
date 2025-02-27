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
         ),
        DT::DTOutput(
          ns("wl_size_table")
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
        title = "WL Reneges",
        p("Second tab content."),
        plotOutput(
          ns("wl_reneging_plot"),
          click = NULL
        )
      ),
      nav_panel(title = "Four", p("Second tab content.")),
      nav_panel(
        title = "WL Capacity",
        p("Second tab content."),
        plotOutput(
          ns("wl_capacity"),
          click = NULL
        )
      ),
      nav_panel(title = "Five", p("Second tab content."))
    )
  )
}

#' 03_results Server Functions
#' @importFrom DT renderDT
#' @import ggplot2
#' @noRd
mod_03_results_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$scenario_projections <- DT::renderDT({

      r$waiting_list

    })

    ## Create waiting list size plot(s)
    output$wl_size <- renderPlot({
      dat<- r$waiting_list |>
        dplyr::summarise(tot = sum(incompletes, na.rm = T),
                         .by = c(period, period_type))

      # determine skew applied from model
      skews <- unique(r$waiting_list$capacity_skew)
      if (length(skews)== 1) {cap_skew <- 1} else {cap_skew <- skews[skews!=1]}

      p <- ggplot2::ggplot() +
        geom_line(data = dplyr::filter(dat, period_type == 'Observed'),
                  aes(x = period,
                      y = tot,
                      group = 1
                  ),  colour = 'black',
                  show.legend = T) +
        geom_line(data = dplyr::filter(dat, period_type == 'Projected'),
                  aes(x = period,
                      y = tot,
                      group = 2
                  ),
                  colour = 'blue',
                  linetype = 'dashed') +
        theme_minimal() +
        ylab('Total patients waiting') +
        xlab(NULL) +
        scale_x_date(breaks = "3 month", minor_breaks = "1 month", date_labels = "%b %y" ) +
        labs(title = paste0 ('Observed and projected waiting list size: ', format(min(dat$period),'%b %Y'), '-', format(max(dat$period),'%b %Y')),
             subtitle = paste0('Based on skew of ', cap_skew))

      p
   }, res = 96)


    output$wl_size_table <- DT::renderDT({

      r$waiting_list |>
        dplyr::arrange (period) |>
        dplyr::mutate(period = format(period,'%m-%y')) |>
        dplyr::summarise(tot = round(sum(incompletes, na.rm = T),0),
                         .by = c(period)) |>
        tidyr::pivot_wider(names_from = period, values_from = tot)

    })

    ## Create performance plots here
    output$wl_performance <- renderPlot({
      perf <- r$waiting_list |>
          dplyr::mutate(target_flag = dplyr::if_else (months_waited_id <= 3, 1, 0)) |>
          dplyr::summarise(tot_wait = sum(incompletes),
                           .by = c(target_flag, period, period_type)) |>
          dplyr::mutate(perf_perc = tot_wait / sum(tot_wait),
                        .by = c(period, period_type)) |>
          dplyr::filter(target_flag == 1)


      ggplot2::ggplot() +
        geom_line(
          data = dplyr::filter(perf, period_type == "Observed"),
          aes(x = period,
              y = perf_perc,
              group = 1),
          colour = "black") +
        geom_line(
          data = dplyr::filter(perf, period_type == "Projected"),
          aes(x = period,
              y = perf_perc,
              group = 2),
          colour = "blue",
          linetype = "dashed"
          ) +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent) +
        scale_x_date(breaks = "3 month",
                     minor_breaks = "1 month",
                     date_labels = "%b %y") +
        ylab("4 Months performance achievement") +
        xlab(NULL) +
        labs(
          title = paste0("Observed and projected 4 month performance: ", format(min(d$period), "%b %Y"), "-", format(max(d$period), "%b %Y")),
          subtitle = paste0("Based on "),
          caption = paste0("Data taken from blah blah on ", format(Sys.Date(), "%d/%m/%Y"))
        )

    }, res = 96)

    ## Create reneging plots
    output$wl_reneging_plot <- renderPlot({

      dat<- r$waiting_list |>
        dplyr::summarise(tot = sum(reneges, na.rm = T),
                         .by = c(period, period_type, months_waited_id))

      # determine skew applied from model
      skews <- unique(r$waiting_list$capacity_skew)
      if (length(skews)== 1) {cap_skew <- 1} else {cap_skew <- skews[skews!=1]}

      p <- ggplot2::ggplot() +
        geom_line(data = dplyr::filter(dat, period_type == 'Observed'),
                  aes(x = period,
                      y = tot,
                      group = 1
                  ),  colour = 'black',
                  show.legend = T) +
        geom_line(data = dplyr::filter(dat, period_type == 'Projected'),
                  aes(x = period,
                      y = tot,
                      group = 2
                  ),
                  colour = 'blue',
                  linetype = 'dashed') +
        theme_minimal() +
        facet_wrap(~months_waited_id, ncol = 4) +
        ylab('Total patients waiting') +
        xlab(NULL) +
        scale_x_date(breaks = "3 month", minor_breaks = "1 month", date_labels = "%b %y" ) +
        labs(title = paste0 ('Observed and projected reneges: ', format(min(dat$period),'%b %Y'), '-', format(max(dat$period),'%b %Y')),
             subtitle = paste0('Based on skew of ', cap_skew))

      p


    }, res = 96)

    ## Create waiting list capacity plot(s)
    output$wl_capacity <- renderPlot({
      dat<- r$waiting_list |>
        dplyr::summarise(tot = sum(calculated_treatments, na.rm = T),
                         .by = c(period, period_type))

      # determine skew applied from model
      skews <- unique(r$waiting_list$capacity_skew)
      if (length(skews)== 1) {cap_skew <- 1} else {cap_skew <- skews[skews!=1]}

      p <- ggplot2::ggplot() +
        geom_line(data = dplyr::filter(dat, period_type == 'Observed'),
                  aes(x = period,
                      y = tot,
                      group = 1
                  ),  colour = 'black',
                  show.legend = T) +
        geom_line(data = dplyr::filter(dat, period_type == 'Projected'),
                  aes(x = period,
                      y = tot,
                      group = 2
                  ),
                  colour = 'blue',
                  linetype = 'dashed') +
        theme_minimal() +
        ylab('Total patients waiting') +
        xlab(NULL) +
        scale_x_date(breaks = "3 month", minor_breaks = "1 month", date_labels = "%b %y" ) +
        labs(title = paste0 ('Observed and projected completed pathways: ', format(min(dat$period),'%b %Y'), '-', format(max(dat$period),'%b %Y')),
             subtitle = paste0('Based on skew of ', cap_skew))

      p
    }, res = 96)

  })
}

## To be copied in the UI
# mod_03_results_ui("03_results_1")

## To be copied in the server
# mod_03_results_server("03_results_1")

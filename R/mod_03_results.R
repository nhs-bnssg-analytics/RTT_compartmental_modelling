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
        title = "One",
        p("Second tab content."),
         plotOutput(
           ns("wl_size"),
           click = "plot_click"
         )
      ),
      nav_panel(
        title = "Two",
        p("Second tab content."),
        plotOutput(
          ns("wl_perf"),
          click = NULL
                )
        ),
      nav_panel(title = "Three", p("Second tab content.")),
      nav_panel(title = "Four", p("Second tab content.")),
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
      ) |>
        DT::formatRound(
          columns = c(
            "calculated_treatments",
            "reneges",
            "incompletes",
            "unadjusted_referrals"
          ),
          digits = 1
        )

    })

    ## Create waiting list plots here
    output$wl_size <- renderPlot({
      dat<- r$waiting_list |>
        dplyr::summarise(tot = sum(incompletes, na.rm = T),
                         .by = period)

     ggplot2::ggplot(dat, aes(x = period,
                                y = tot)) +
       geom_point() +
       geom_line() +
       theme_minimal()
   }, res = 96)

    ## Create perfromance plots here
    output$wl_perf <- renderPlot({
      # dat2<- r$waiting_list |>
      #   dplyr::summarise(tot = sum(incompletes, na.rm = T),
      #                  .by = c(period, months_waited_id)) |>
      #   dplyr::mutate(test_col = dplyr::if_else (months_waited_id <= 3, 1, 0))
      #
      # ggplot2::ggplot(dat2, aes(x = period,
      #                          y = tot)) +
      #   geom_point() +
      #   geom_line() +
      #   theme_minimal()

      # dat2$target_flag <- ifelse (dat2$month_waited_id <= 3, 1, 0)
      #
      # dat2 <- dat2 |>
      #       dplyr::summarise(tot_wait = sum(incompletes),
      #                        .by = c(target_flag, period)) |>
      #       dplyr::mutate(perf = tot_wait / sum(tot_wait),
      #                     .by = c(period)) |>
      #       dplyr::filter(target_flag == 1)
      #



        # ggplot2::ggplot(dat2, aes(x = period,
        #                        y = tot)) +
        # geom_point() +
        # geom_line() +
        # theme_minimal()

      #
      # per <- r$waiting_list |>
      #     dplyr::mutate(target_flag = dplyr::if_else (month_waited_id <= 3, 1, 0)) |>
      #     dplyr::summarise(tot_wait = sum(incompletes),
      #                      .by = c(target_flag, period)) |>
      #     dplyr::mutate(perf = tot_wait / sum(tot_wait),
      #                   .by = c(period)) |>
      #     dplyr::filter(target_flag == 1)

#
#       ggplot2::ggplot(performance, aes(x= period, y = perf)) +
#         geom_point() +
#         geom_line() +
#         theme_minimal() +
#         scale_y_continuous(labels = scales::percent)
    }, res = 96)

  })
}

## To be copied in the UI
# mod_03_results_ui("03_results_1")

## To be copied in the server
# mod_03_results_server("03_results_1")

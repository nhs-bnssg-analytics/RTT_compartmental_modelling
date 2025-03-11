#' Visualisation functions for module 3
#' @description Generate a time series plot of observed and projected performance.
#'
#' This function creates a ggplot2 time series plot showing observed and projected
#' performance data, with options to customise the plot based on different scenario
#' parameters. It handles various scenarios, including estimating performance from
#' capacity inputs and optimising capacity to achieve a target performance.
#'
#' @param data A data frame containing the data to be plotted. It should include columns
#'   'period', 'period_type' (either "Observed" or "Projected"), and 'p_var' (the variable
#'   to be plotted).
#' @param p_trust A character string specifying the trust name.
#' @param p_speciality A character string specifying the speciality.
#' @param p_chart A character string describing the chart type
#'    or variable being plotted.
#' @param p_scenario A character string specifying the scenario type, either
#'   "Estimate performance (from capacity inputs)" or another scenario (e.g., "Optimise Capacity").
#' @param p_cap_change A numeric value representing the percentage
#'    change in capacity.
#' @param p_cap_change_type A character string describing the type of capacity
#'    change (e.g., "linear", "uniform").
#' @param p_cap_skew A numeric value representing the utilisation skew factor.
#' @param p_target_performance A numeric value representing the target
#'    performance (used in capacity optimisation).
#' @param p_target_date A date object representing the target
#'    date (used in capacity optimisation).
#' @param p_referrals_percent_change A numeric value representing the
#'    percentage change in referrals.
#' @param p_referrals_change_type A character string describing the type of
#'    referrals change (e.g., "linear", "uniform").
#' @param p_perc A logical value indicating whether the y-axis should be
#'    formatted as percentages.
#' @param p_facet A logical value indicating whether to facet the plot
#'    by 'months_waited_id'.
#' @param p_target_line A logical value indicating whether to include target
#'    line and change colour of "target" in subheading
#'
#' @importFrom dplyr filter distinct rename left_join join_by
#' @importFrom ggtext element_markdown
#' @importFrom scales percent comma
#' @importFrom rlang .data
#' @import ggplot2
#' @return A ggplot2 plot object of selected values
#' @noRd

plot_output <- function(data,
                        p_trust,
                        p_speciality,
                        p_chart,
                        p_scenario,
                        p_cap_change = 0,
                        p_cap_change_type,
                        p_cap_skew,
                        p_target_performance,
                        p_target_date,
                        p_referrals_percent_change,
                        p_referrals_change_type,
                        p_perc,
                        p_facet = F,
                        p_target_line = F) {
  p <- ggplot2::ggplot() +
    geom_step(
      data = dplyr::filter(data, .data$period_type == "Observed"),
      aes(
        x = .data$period,
        y = .data$p_var,
        group = 1
      ), colour = "black",
      show.legend = T
    ) +
    geom_step(
      data = dplyr::filter(data, .data$period_type == "Projected"),
      aes(
        x = .data$period,
        y = .data$p_var,
        group = 2
      ),
      colour = "blue",
      linetype = "dashed"
    ) +
    theme_minimal() +
    xlab(NULL)

if (p_scenario == "Estimate performance (from capacity inputs)") {
  p <- p +
    labs(
      title = paste0("<b>", p_trust, "</b> : ", p_speciality),
      subtitle = paste0(
        "<span style='color:black'>**Observed**</span><span style='color:#425563'> and </span><span style='color:blue'>**projected** </span><span style='color:#425563'>", p_chart, ": ", format(min(data$period), "%b %Y"), "-", format(max(data$period), "%b %Y"),
        "<br>Performance based on a ", p_cap_change_type, " capacity change of ", p_cap_change, "% with a utilisation skew factor of ", p_cap_skew,
        "<br>Referrals ", p_referrals_change_type, "ly adjusted by ", p_referrals_percent_change, "% </span>"
      ),
      caption = paste0("Data taken from www.england.nhs.uk/statistics/statisical-work-areas/rtt-waiting-times - ", format(Sys.Date(), "%d/%m/%Y"))
    ) +
    theme(
      plot.title = ggtext::element_markdown(),
      plot.subtitle = ggtext::element_markdown()
    )
} else if (p_target_line == F & p_scenario == 'Estimate capacity (from performance targets)') {
  p <- p +
    labs(
      title = paste0("<b>",p_trust, "</b> : ", p_speciality),
      subtitle = paste0(
        "<span style='color:black'>**Observed**</span><span style='color:#425563'> and </span><span style='color:blue'>**projected** </span><span style='color:#425563'>", p_chart, ": ", format(min(data$period), "%b %Y"), "-", format(max(data$period), "%b %Y"),
        "<br>Optimised capacity with a utilisation skew factor of ", p_cap_skew, " to achieve a target of ", p_target_performance, "% patients seen within 4 months by ", p_target_date,
        "<br>Referrals ", p_referrals_change_type, "ly adjusted by ", p_referrals_percent_change, "%</span>"
      ),
      caption = paste0("Data taken from www.england.nhs.uk/statistics/statisical-work-areas/rtt-waiting-times - ",
                       format(Sys.Date(), "%d/%m/%Y"))
    ) +
    theme(
      plot.title = ggtext::element_markdown(),
      plot.subtitle = ggtext::element_markdown()
      )
    } else {
      p <- p +
        labs(
          title = paste0("<b>",p_trust, "</b> : ", p_speciality),
          subtitle = paste0(
            "<span style='color:black'>**Observed**</span><span style='color:#425563'> and </span><span style='color:blue'>**projected** </span><span style='color:#425563'>", p_chart, ": ", format(min(data$period), "%b %Y"), "-", format(max(data$period), "%b %Y"),
            "<br>Optimised capacity with a utilisation skew factor of ", p_cap_skew, " to achieve a <span style='color:red'>**target**</span> of ", p_target_performance, "% patients seen within 4 months by ", format(p_target_date,"%b %Y") ,
            "<br>Referrals ", p_referrals_change_type, "ly adjusted by ", p_referrals_percent_change, "%</span>"
          ),
          caption = paste0("Data taken from www.england.nhs.uk/statistics/statisical-work-areas/rtt-waiting-times - ",
                           format(Sys.Date(), "%d/%m/%Y"))
        ) +
        theme(
          plot.title = ggtext::element_markdown(),
          plot.subtitle = ggtext::element_markdown()
        )
    }

    if (p_perc == T ) {
      p <- p +
        scale_y_continuous(labels = scales::percent) +
        ylab('Percent')

    } else {
      p <- p +
        scale_y_continuous(labels = scales::comma) +
        ylab('Number of patients')
    }

    if (p_facet == T ) {
      p <- p +
        facet_wrap(~months_waited_id, ncol = 4) +
        scale_x_date(breaks = "6 month",
                     minor_breaks = "2 month",
                     date_labels = "%b %y")
    } else {
      p <- p +
        scale_x_date(breaks = "3 month",
                     minor_breaks = "1 month",
                     date_labels = "%b %y")
    }

   if (p_target_line == T & p_scenario == 'Estimate capacity (from performance targets)') {
      p <- p +
       geom_hline(yintercept = p_target_performance/100,
                  colour = 'red',
                  linetype = 'dotted')
   }


  p
}


plot_skew <- function(params, skew_values, pivot_bin, skew_method) {

  original <- params |>
    dplyr::select(
      "months_waited_id",
      "capacity_param"
    ) |>
    dplyr::mutate(
      scenario = "Original"
    )

  if (length(skew_values) == 1) {
    skew_values <- setNames(
      skew_values,
      nm = "Skewed"
    )
    col_palette <- c(
      "Original" = "#1E88E5",
      "Skewed" = "#FFC107"
    )
  } else if (length(skew_values) == 2) {
    skew_values <- sort(skew_values)
    skew_values <- setNames(
      skew_values,
      nm = c("Low skew", "High skew")
    )
    col_palette <- c(
      "Original" = "#1E88E5",
      "Low skew" = "#D81B60",
      "High skew" = "#004D40"
    )

  } else {
    stop("skew_values must have length 1 or 2")
  }



  skewed <- skew_values |>
    purrr::imap(
      \(x, idx) dplyr::tibble(
        months_waited_id = params$months_waited_id,
        capacity_param = NHSRtt::apply_parameter_skew(
          params$capacity_param,
          skew = x,
          skew_method = skew_method,
          pivot_bin = pivot_bin
        ),
        scenario = idx
      )
    ) |>
    purrr::list_rbind()

  p_skews <- dplyr::bind_rows(
    original,
    skewed
  ) |>
    ggplot(
      aes(
        x = factor(
          .data$months_waited_id,
          levels = .data$months_waited_id
        ),
        y = .data$capacity_param
      )
    ) +
    geom_line(
      aes(
        group = .data$scenario,
        colour = .data$scenario
      )
    ) +
    theme_bw() +
    scale_colour_manual(
      name = "",
      values = col_palette
    ) +
    labs(
      x = "Stock (# mnths waited)",
      y = "Clock stop rate"
    )

  return(p_skews)
}

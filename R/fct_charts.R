#' Visualisation functions for module 3
#' @description Generate a time series plot of observed and projected
#'   performance.
#'
#'   This function creates a ggplot2 time series plot showing observed and
#'   projected performance data, with options to customise the plot based on
#'   different scenario parameters. It handles various scenarios, including
#'   estimating performance from capacity inputs and optimising capacity to
#'   achieve a target performance.
#'
#' @param data A data frame containing the data to be plotted. It should include
#'   columns 'period', 'period_type' (either "Observed" or "Projected"), and
#'   'p_var' (the variable to be plotted).
#' @param p_trust A character string specifying the trust name.
#' @param p_speciality A character string specifying the speciality.
#' @param p_chart A character string describing the chart type or variable being
#'   plotted.
#' @param p_scenario A character string specifying the scenario type, either
#'   "Estimate performance (from capacity inputs)" or another scenario (e.g.,
#'   "Optimise Capacity").
#' @param p_cap_change A numeric value representing the percentage change in
#'   capacity.
#' @param p_cap_change_type A character string describing the type of capacity
#'   change (e.g., "linear", "uniform").
#' @param p_cap_skew A numeric value representing the utilisation skew factor.
#' @param target_data A table with two columns: Target_date and
#'   Target_percentage, containing entries for multiple dates and performance
#'   targets.
#' @param p_referrals_percent_change A numeric value representing the percentage
#'   change in referrals.
#' @param p_referrals_change_type A character string describing the type of
#'   referrals change (e.g., "linear", "uniform").
#' @param p_perc A logical value indicating whether the y-axis should be
#'   formatted as percentages.
#' @param p_facet A logical value indicating whether to facet the plot by
#'   'months_waited_id'.
#' @param p_target_line A logical value indicating whether to include target
#'   line and change colour of "target" in subheading
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
                        p_target_data,
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

    txt <- performance_text(p_target_data)

    p <- p +
      labs(
        title = paste0("<b>",p_trust, "</b> : ", p_speciality),
        subtitle = paste0(
          "<span style='color:black'>**Observed**</span><span style='color:#425563'> and </span><span style='color:blue'>**projected** </span><span style='color:#425563'>",
          p_chart, ": ", format(min(data$period), "%b %Y"), "-", format(max(data$period), "%b %Y"),
          "<br>Optimised capacity with a utilisation skew factor of ", p_cap_skew, " to achieve a target of ", txt, " of patients seen within 4 months",
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

    txt <- performance_text(p_target_data)
    p <- p +
      labs(
        title = paste0("<b>",p_trust, "</b> : ", p_speciality),
        subtitle = paste0(
          "<span style='color:black'>**Observed**</span><span style='color:#425563'> and </span><span style='color:blue'>**projected** </span><span style='color:#425563'>",
          p_chart, ": ", format(min(data$period), "%b %Y"), "-", format(max(data$period), "%b %Y"),
          "<br>Optimised capacity with a utilisation skew factor of ", p_cap_skew, " to achieve a <span style='color:red'>**target**</span> of ", txt, " of patients seen within 4 months",
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
      geom_hline(
        data = p_target_data,
        aes(
          yintercept = .data$Target_percentage / 100
        ),
        colour = 'red',
        linetype = 'dotted')
  }


  p
}


plot_skew <- function(params, skew_values, pivot_bin, skew_method) {

  if (is.null(params)) {
    return(ggplot())
  }

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
          levels = params$months_waited_id
        ),
        y = .data$capacity_param
      )
    ) +
    geom_step(
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

#' geom_step in the charts do not display the final observed or projected months
#' well because the stepped line terminates at the start of the month. This
#' function adds an artificial month onto the observed and projected
#' period_types so they are displayed better on the visualisations
#' @param plot_data tibble containing the columns period and period_type (which
#'   contains values "Observed" and "Projected")
#' @importFrom dplyr filter mutate bind_rows
extend_period_type_data <- function(plot_data) {
  additional_month <- plot_data |>
    filter(
      .data$period == max(.data$period),
      .by = "period_type"
    ) |>
    mutate(
      period = .data$period %m+% months(1)
    )

  plot_data <- plot_data |>
    bind_rows(
      additional_month
    )

  return(plot_data)
}

#' function to return the data behind where the user has clicked
#' @param data the data underpinning the chart that has been clicked
#' @param click_x the x location where the click occurred
#' @param facet the months_waited_id that the data was selected from when the
#'   plots are faceted
click_info <- function(data, click_x, facet = NULL) {
  x_val <- as.Date(click_x)

  # Find nearest data point
  nearest_idx <- data |>
    ungroup() |>
    dplyr::filter(
      x_val - .data$period > 0
    ) |>
    dplyr::filter(
      x_val - .data$period == min(x_val - .data$period)
    )

  if (nrow(nearest_idx) > 1) {
    # this will only occur for the first period after the observed data finishes
    # because it is artificially extended so it is displayed better on the
    # charts
    nearest_idx <- nearest_idx |>
      filter(.data$period_type == "Projected")
  }

  if (!is.null(facet)) {
    nearest_idx <- nearest_idx |>
      filter(
        .data$months_waited_id == facet
      )
  }

  return(nearest_idx)
}

#' @importFrom utils head tail
performance_text <- function(p_target_data) {

  p_target_data <- p_target_data |>
    mutate(
      Target_date = format(.data$Target_date, "%b %Y"),
      final_text = paste0(.data$Target_percentage, "% (", .data$Target_date, ")")
    )

  if (nrow(p_target_data) == 1) {
    p_out <- p_target_data[["final_text"]]
  } else if (nrow(p_target_data) == 2) {
    p_out <- paste0(
      p_target_data[["final_text"]],
      collapse = " and "
    )
  } else {
    p_out <- paste0(
      paste0(
        head(p_target_data[["final_text"]], nrow(p_target_data) - 1),
        ", ",
        collapse = ""
      ),
      "and ",
      tail(p_target_data[["final_text"]], 1)
    ) |>
      (\(x) gsub(", and", " and", x))()

  }

  txt <- p_out

  return(txt)
}



# tooltip functions -------------------------------------------------------

linear_tooltip <- function() {
  dplyr::tibble(
    period = 1:10,
    value = c(rep(1, 5), 2:6)
  ) |>
    ggplot(
      aes(x = period,
          y = value)
    ) +
    geom_line() +
    geom_vline(
      xintercept = 5.5,
      linetype = "dashed"
    ) +
    geom_text(
      data = dplyr::tibble(
        period = 6,
        value = Inf,
        label = "Forecast"
      ),
      aes(
        label = label
      ),
      hjust = 0,
      vjust = 1.2
    ) +
    theme_linedraw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(
        fill = "#FAE100",
        colour= "#FAE100"
      ),
      plot.background = element_rect(
        fill = "#FAE100",
        colour= "#FAE100"
      )
    ) +
    ylim(0, 8)
}

uniform_tooltip <- function() {
  dplyr::tibble(
    period = 1:10,
    value = c(rep(1, 5), rep(4, 5))
  ) |>
    ggplot(
      aes(x = period,
          y = value)
    ) +
    geom_line() +
    geom_vline(
      xintercept = 5.5,
      linetype = "dashed"
    ) +
    geom_text(
      data = dplyr::tibble(
        period = 6,
        value = Inf,
        label = "Forecast"
      ),
      aes(
        label = label
      ),
      hjust = 0,
      vjust = 1.2
    ) +
    theme_linedraw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(
        fill = "#FAE100",
        colour= "#FAE100"
      ),
      plot.background = element_rect(
        fill = "#FAE100",
        colour= "#FAE100"
      )
    ) +
    ylim(0, 8)
}


linear_uniform_tooltip <- function(uniform_id, linear_id) {

  div(
    shiny::HTML(
      paste0(
        "<strong>Uniform:</strong> ",
        "Treatment capacity/referral change occurs in first month and remains flat for the whole 'Forecast horizon' period.<br><br>"
      )
    ),
    plotOutput(
      outputId = uniform_id,
      height = "75px"
    ),
    shiny::HTML(
      paste0(
        "<strong>Linear:</strong> ",
        "The first month of the 'Forecast horizon' period is estimated from the historic data, and then treatment capacity/referral is changed linearly until the end of the 'Forecast horizon'."
      )
    ),
    plotOutput(
      outputId = linear_id,
      height = "75px"
    )
  )
}

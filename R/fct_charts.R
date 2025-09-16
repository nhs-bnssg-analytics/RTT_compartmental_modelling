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
#'   capacity. This can also take a string to accommodate a customised input
#'  for capacity change.
#' @param p_cap_change_type A character string describing the type of capacity
#'   change (e.g., "linear", "uniform").
#' @param p_cap_skew A numeric value representing the utilisation skew factor.
#' @param target_data A table with two columns: Target_date and
#'   Target_percentage, containing entries for multiple dates and performance
#'   targets.
#' @param p_referrals_percent_change A numeric value representing the percentage
#'   change in referrals. This can also take a string to accommodate a customised
#'  input for referrals change.
#' @param p_referrals_change_type A character string describing the type of
#'   referrals change (e.g., "linear", "uniform").
#' @param p_perc A logical value indicating whether the y-axis should be
#'   formatted as percentages.
#' @param p_facet A logical value indicating whether to facet the plot by
#'   'months_waited_id'.
#' @param p_target_line A logical value indicating whether to include target
#'   line and change colour of "target" in subheading
#' @param date_input date for chart caption
#'
#' @importFrom dplyr filter distinct rename left_join join_by tibble cross_join
#' @importFrom ggtext element_markdown
#' @importFrom scales percent comma
#' @importFrom rlang .data
#' @importFrom lubridate `%m+%`
#' @import ggplot2
#' @return A ggplot2 plot object of selected values
#' @noRd

plot_output <- function(
  data,
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
  p_target_line = F,
  date_input = Sys.Date()
) {
  if (is.numeric(p_referrals_percent_change)) {
    p_referrals_percent_change <- paste0(
      "by ",
      p_referrals_percent_change,
      "%"
    )
  } else {
    p_referrals_percent_change
  }
  if (is.numeric(p_cap_change)) {
    p_cap_change <- paste0(
      "of ",
      p_cap_change,
      "%"
    )
  } else {
    p_cap_change
  }

  p <- ggplot2::ggplot() +
    geom_vline(
      data = dplyr::filter(
        data,
        months.Date(.data$period) == "January"
      ),
      aes(
        xintercept = .data$period
      ),
      alpha = 0.3
    ) +
    geom_step(
      data = dplyr::filter(data, .data$period_type == "Observed"),
      aes(
        x = .data$period,
        y = .data$p_var,
        group = 1
      ),
      colour = "black",
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

  if (p_scenario == "Estimate performance (from treatment capacity inputs)") {
    p <- p +
      labs(
        title = paste0("<b>", p_trust, "</b> : ", p_speciality),
        subtitle = paste0(
          "<span style='color:black'>**Observed**</span><span style='color:#425563'> and </span><span style='color:blue'>**projected** </span><span style='color:#425563'>",
          p_chart,
          ": ",
          format(min(data$period), "%b %Y"),
          "-",
          format(max(data$period), "%b %Y"),
          "<br>Performance based on a ",
          p_cap_change_type,
          " treatment capacity change ",
          p_cap_change,
          " with a utilisation skew factor of ",
          p_cap_skew,
          "<br>Referrals ",
          p_referrals_change_type,
          "ly adjusted ",
          p_referrals_percent_change,
          " </span>"
        ),
        caption = paste0(
          "Data taken from www.england.nhs.uk/statistics/statisical-work-areas/rtt-waiting-times - ",
          format(date_input, "%d/%m/%Y")
        )
      ) +
      theme(
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown()
      )
  } else if (
    p_target_line == F &
      p_scenario == 'Estimate treatment capacity (from performance targets)'
  ) {
    txt <- performance_text(p_target_data)

    p <- p +
      labs(
        title = paste0("<b>", p_trust, "</b> : ", p_speciality),
        subtitle = paste0(
          "<span style='color:black'>**Observed**</span><span style='color:#425563'> and </span><span style='color:blue'>**projected** </span><span style='color:#425563'>",
          p_chart,
          ": ",
          format(min(data$period), "%b %Y"),
          "-",
          format(max(data$period), "%b %Y"),
          "<br>Optimised treatment capacity with a utilisation skew factor of ",
          p_cap_skew,
          " to achieve a target of ",
          txt,
          " of patients seen within 18 weeks",
          "<br>Referrals ",
          p_referrals_change_type,
          "ly adjusted ",
          p_referrals_percent_change,
          "</span>"
        ),
        caption = paste0(
          "Data taken from www.england.nhs.uk/statistics/statisical-work-areas/rtt-waiting-times - ",
          format(date_input, "%d/%m/%Y")
        )
      ) +
      theme(
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown()
      )
  } else {
    txt <- performance_text(p_target_data)
    p <- p +
      labs(
        title = paste0("<b>", p_trust, "</b> : ", p_speciality),
        subtitle = paste0(
          "<span style='color:black'>**Observed**</span><span style='color:#425563'> and </span><span style='color:blue'>**projected** </span><span style='color:#425563'>",
          p_chart,
          ": ",
          format(min(data$period), "%b %Y"),
          "-",
          format(max(data$period), "%b %Y"),
          "<br>Optimised treatment capacity with a utilisation skew factor of ",
          p_cap_skew,
          " to achieve a <span style='color:red'>**target**</span> of ",
          txt,
          " of patients seen within 18 weeks",
          "<br>Referrals ",
          p_referrals_change_type,
          "ly adjusted ",
          p_referrals_percent_change,
          "</span>"
        ),
        caption = paste0(
          "Data taken from www.england.nhs.uk/statistics/statisical-work-areas/rtt-waiting-times - ",
          format(date_input, "%d/%m/%Y")
        )
      ) +
      theme(
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown()
      )
  }

  if (p_perc == T) {
    p <- p +
      scale_y_continuous(labels = scales::percent) +
      ylab('Percent')
  } else {
    p <- p +
      scale_y_continuous(labels = scales::comma) +
      ylab('Number of patients')
  }

  if (p_facet == T) {
    p <- p +
      facet_wrap(~months_waited_id, ncol = 4) +
      scale_x_date(
        breaks = january_breaks_facetted,
        date_labels = "%b\n%Y"
      )
  } else {
    p <- p +
      scale_x_date(
        breaks = january_breaks,
        date_labels = "%b\n%Y"
      )
  }

  if (
    p_target_line == T &
      p_scenario == 'Estimate treatment capacity (from performance targets)'
  ) {
    # create the target month table
    target_month <- dplyr::tibble(
      start_date = p_target_data[["Target_date"]]
    ) |>
      mutate(
        end_date = (.data$start_date %m+% months(1)) - 1,
        group = dplyr::row_number()
      ) |>
      dplyr::cross_join(
        dplyr::tibble(
          y = c(-Inf, Inf)
        )
      )

    p_target_data_chart <- p_target_data |>
      mutate(
        Target_text = paste0(
          format(.data$Target_date, format = "%b %y"),
          " performance target: ",
          .data$Target_percentage,
          "%"
        ),
        Target_date = (.data$Target_date %m+% months(1)) - 1
      )

    p <- p +
      geom_ribbon(
        data = target_month,
        aes(
          y = .data$y,
          group = .data$group,
          xmin = .data$start_date,
          xmax = .data$end_date
        ),
        alpha = 0.5,
        fill = "gray45"
      ) +
      geom_text(
        data = p_target_data_chart,
        aes(
          label = .data$Target_text,
          x = .data$Target_date
        ),
        y = -Inf,
        vjust = -0.12,
        hjust = -0.025,
        angle = 90,
        alpha = 0.8
      ) +
      geom_hline(
        data = p_target_data,
        aes(
          yintercept = .data$Target_percentage / 100
        ),
        colour = 'red',
        linetype = 'dotted'
      )
  }

  return(p)
}

#' @param type one of "model" (advising user to return to modelling page) or
#'   "select_chart" (advising user to select the chart)
#' @importFrom dplyr tibble
#' @importFrom rlang .data
#' @import ggplot2
#' @noRd
holding_chart <- function(type) {
  type <- match.arg(
    type,
    c("model", "select_chart")
  )

  if (type == "model") {
    holding_text <- "Please return to the 'Scenario planner' tab to create some modelled data"
  } else if (type == "select_chart") {
    holding_text <- "Please make chart selection on the sidebar"
  }

  ggplot() +
    geom_text(
      data = dplyr::tibble(
        x = 1,
        y = 1,
        label = holding_text
      ),
      aes(
        x = .data$x,
        y = .data$y,
        label = .data$label
      ),
    ) +
    theme_void()
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
      \(x, idx) {
        dplyr::tibble(
          months_waited_id = params$months_waited_id,
          capacity_param = NHSRtt::apply_parameter_skew(
            params$capacity_param,
            skew = x,
            skew_method = skew_method,
            pivot_bin = pivot_bin
          ),
          scenario = idx
        )
      }
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
      x = "Number of months waited",
      y = "Treatment capacity rate"
    )

  return(p_skews)
}

#' create plot of observed and modelled performance for the calibration period
#' @param modelled_data data frame containing the modelled and observed waiting list for the second part of the calibration period
#' @param observed_data data frame containing the observed waiting list
#' @importFrom dplyr bind_rows filter mutate summarise
#' @importFrom lubridate `%m+%`
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @import ggplot2
#' @noRd
plot_error <- function(modelled_data, observed_data) {
  observed_dates <- range(observed_data$period)

  p <- modelled_data |>
    tidyr::pivot_longer(
      cols = c("modelled_incompletes", "original"),
      names_to = "type",
      values_to = "value"
    ) |>
    dplyr::bind_rows(
      observed_data
    ) |>
    mutate(
      type = case_when(
        type %in% c("original", "Incomplete") ~ "Observed",
        type == "modelled_incompletes" ~ "Modelled"
      ),
      perf = case_when(
        .data$months_waited_id < 4 ~ "Below",
        .default = "Above"
      )
    ) |>
    summarise(
      value = sum(.data$value),
      .by = c(
        "period",
        "type",
        "perf"
      )
    ) |>
    mutate(
      prop = 100 * (.data$value / sum(.data$value)),
      .by = c(
        "period",
        "type"
      )
    ) |>
    filter(
      .data$perf == "Below"
    ) |>
    # repeat the final period of data, but artificially apply it to the subsequent month
    (\(x) {
      x |>
        filter(.data$period == max(.data$period)) |>
        mutate(period = period %m+% months(1)) |>
        bind_rows(x)
    })() |>
    ggplot2::ggplot(
      aes(
        x = .data$period,
        y = .data$prop,
        group = .data$type
      )
    ) +
    geom_rect(
      data = tibble(
        date_start = observed_dates[1],
        date_end = lubridate::ceiling_date(
          observed_dates[2],
          unit = "month"
        )
      ),
      aes(
        xmin = .data$date_start,
        xmax = .data$date_end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "grey",
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    geom_text(
      x = observed_dates[1],
      y = Inf,
      label = "For error calculation, model calibration period is shaded in grey",
      hjust = 0,
      vjust = 2,
      inherit.aes = FALSE
    ) +
    geom_step(
      aes(
        colour = .data$type,
        linetype = .data$type
      )
    ) +
    theme_minimal() +
    # make Modelled dashed and Observed solid linetype
    scale_linetype_manual(
      name = "",
      values = c(
        "Observed" = "solid",
        "Modelled" = "dashed"
      )
    ) +
    # make Modelled and Observed colours different
    scale_colour_manual(
      name = "",
      values = c(
        "Observed" = "black",
        "Modelled" = "blue"
      )
    ) +
    labs(
      title = "Modelled and observed 18 week performance for the calibration period",
      y = "18 week performance (%)",
      x = ""
    ) +
    # put black border around the whole plot
    theme(
      plot.background = element_rect(
        fill = NA,
        colour = "black"
      ),
      legend.position = "bottom"
    )

  return(p)
}


#' Plot Waiting List Distributions with Target Indicators
#'
#' Generates a faceted bar plot showing the distribution of waiting list sizes
#' across different categories, with vertical lines and labels indicating a target
#' number of weeks and a percentile threshold.
#'
#' @param data A data frame containing waiting list information. Must include columns:
#'   - `months_waited_id`: numeric identifier for months waited
#'   - `wlsize`: number of people on the waiting list
#'   - `sigma`: the number of treatments for each compartment
#'   - `wl_description`: description used for faceting
#' @param target_week Numeric value indicating the target number of weeks to wait.
#'   This will be converted to months and shown as a vertical dashed line with a label.
#' @param target_value Numeric value representing the percentile (e.g., 90 for 90th percentile).
#'   Used for labeling the percentile line.
#'
#' @return A `ggplot2` object representing the waiting list distribution plot.
#'
#' @examples
#' \dontrun{
#' plot_waiting_lists_chart(data = my_data,
#'                    target_week = 18,
#'                    target_value = 90)
#' }
#'
#' @import ggplot2
#' @export
plot_waiting_lists_chart <- function(
  data,
  target_week,
  target_value
) {
  # browser()
  percentile_calculation <- data |>
    select(
      "trust",
      "specialty",
      "referrals_scenario",
      "wl_description",
      "months_waited_id",
      "wlsize"
    ) |>
    mutate(wlsize = tidyr::replace_na(.data$wlsize, 0)) |>
    tidyr::nest(wl_shape = c("months_waited_id", "wlsize")) |>
    mutate(
      target_percentile = purrr::map_dbl(
        .data$wl_shape,
        ~ NHSRtt::hist_percentile_calc(
          wl_structure = .x,
          percentile = target_value / 100
        )
      ),
      percentile_at_target = purrr::map_dbl(
        .data$wl_shape,
        ~ calc_percentile_at_week(
          wl_shape = .x,
          week = target_week
        )
      ),
      percentile_between_target_week_and_target_percentile = (target_value /
        100) -
        .data$percentile_at_target,
      percentile_above_target_value = 1 -
        (.data$percentile_at_target +
          .data$percentile_between_target_week_and_target_percentile),
      percentile_between_target_week_and_target_percentile = case_when(
        round(.data$percentile_between_target_week_and_target_percentile, 1) ==
          0 ~
          NA_real_,
        .default = .data$percentile_between_target_week_and_target_percentile
      ),
      facet_join = "Incomplete"
    ) |>
    select(!c("wl_shape"))

  segment_y = -max(data$wlsize) * 0.05
  text_y = -max(data$wlsize) * 0.1

  segment_data <- percentile_calculation |>
    dplyr::cross_join(
      dplyr::tibble(
        status = factor(
          c("Within", "Between", "Above"),
          levels = c("Within", "Between", "Above")
        )
      )
    ) |>
    mutate(
      x_start = case_when(
        .data$status == "Within" ~ -0.5,
        .data$status == "Between" ~ convert_weeks_to_months(target_week) - 0.5,
        .data$status == "Above" ~
          ifelse(
            .data$target_percentile - 0.5 <
              convert_weeks_to_months(target_week) - 0.5,
            convert_weeks_to_months(target_week) - 0.5,
            .data$target_percentile - 0.5
          ),
      ),
      x_end = case_when(
        .data$status == "Within" ~ convert_weeks_to_months(target_week) - 0.5,
        .data$status == "Between" ~
          ifelse(
            .data$target_percentile - 0.5 < .data$x_start,
            .data$x_start,
            .data$target_percentile - 0.5
          ),
        .data$status == "Above" ~ 12 + 0.5
      ),
      x_label = (.data$x_start + .data$x_end) / 2,
      y_label = text_y,
      label = case_when(
        .data$status == "Within" ~
          paste0(
            formatC(100 * .data$percentile_at_target, format = "f", digits = 1),
            "%",
            " (",
            target_week,
            " weeks)"
          ),
        .data$status == "Between" ~
          ifelse(
            is.na(.data$percentile_between_target_week_and_target_percentile) |
              .data$percentile_between_target_week_and_target_percentile < 0,
            NA_character_,
            paste0(
              formatC(
                100 *
                  .data$percentile_between_target_week_and_target_percentile,
                format = "f",
                digits = 1
              ),
              "%"
            )
          ),
        .data$status == "Above" ~
          ifelse(
            .data$percentile_at_target == 1,
            "0.0%",
            paste0(
              formatC(
                100 * .data$percentile_above_target_value,
                format = "f",
                digits = 1
              ),
              "%"
            )
          )
      ),
      y_start_end = segment_y
    )

  rect_data <- segment_data |>
    select("wl_description", "facet_join", "status", "x_end", "x_start") |>
    mutate(facet_join = "Treatment") |>
    bind_rows(segment_data)

  p <- data |>
    pivot_longer(
      cols = c("wlsize", "sigma"),
      names_to = "facet_join",
      names_transform = \(x) {
        case_when(
          x == "wlsize" ~ "Incomplete",
          x == "sigma" ~ "Treatment"
        ) |>
          factor(levels = c("Incomplete", "Treatment"))
      },
      values_to = "value"
    ) |>
    left_join(
      percentile_calculation,
      by = c(
        "trust",
        "specialty",
        "referrals_scenario",
        "wl_description",
        "facet_join"
      )
    ) |>
    ggplot(
      aes(
        x = .data$months_waited_id,
        y = .data$value
      )
    ) +
    geom_rect(
      data = rect_data,
      aes(
        xmin = .data$x_start,
        xmax = .data$x_end,
        fill = .data$status
      ),
      inherit.aes = FALSE,
      ymin = 0,
      ymax = Inf,
      color = NA,
      alpha = 0.6
    ) +
    geom_col(
      fill = "#a3a3a3ff",
      colour = "black"
    ) +
    geom_segment(
      data = segment_data,
      aes(
        x = .data$x_start,
        xend = .data$x_end,
        colour = .data$status,
        y = .data$y_start_end,
        yend = .data$y_start_end
      ),
      linewidth = 1,
      show.legend = FALSE
    ) +
    geom_text(
      data = segment_data,
      aes(
        x = .data$x_label,
        y = .data$y_label,
        colour = .data$status,
        label = .data$label
      ),
      show.legend = FALSE
    ) +
    geom_segment(
      data = percentile_calculation,
      aes(
        x = .data$target_percentile - 0.5,
        xend = .data$target_percentile - 0.5,
        y = 0,
        yend = Inf
      ),
      linetype = "dashed"
    ) +
    geom_text(
      data = percentile_calculation,
      aes(x = .data$target_percentile - 0.5),
      y = Inf,
      angle = 90,
      label = paste0(target_value, "%ile"),
      vjust = 1.5,
      hjust = 1.5
    ) +
    theme_bw(base_size = 15) +
    labs(
      x = "In the nth month of waiting",
      y = "Number of people"
    ) +
    facet_grid(
      cols = vars(.data$wl_description),
      rows = vars(.data$facet_join),
      scales = "free_y",
      switch = "y"
    ) +
    scale_x_continuous(
      breaks = 0:12,
      labels = \(x) ifelse(x == max(x), paste0(x + 1, "+"), x + 1)
    ) +
    scale_colour_manual(
      name = "",
      values = c(
        Within = "#009E73",
        Between = "#E69F00",
        Above = "#D55E00"
      )
    ) +
    scale_fill_manual(
      name = "Percentage of patients waiting",
      values = c(
        Within = "#009E73",
        Between = "#E69F00",
        Above = "#D55E00"
      ),
      labels = c(
        Within = "Within the target timeframe",
        Between = "Between the target timeframe\nand the target percentile",
        Above = "Longer than the target percentile"
      )
    ) +
    theme(legend.position = "bottom")

  return(p)
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

calc_breaks <- function(limits, facetted) {
  years_in_data <- length(
    seq(
      from = limits[1],
      to = limits[2],
      by = "year"
    )
  ) -
    1

  if (years_in_data <= 4) {
    labels_per_year <- 4
  } else if (years_in_data <= 8) {
    labels_per_year <- 2
  } else {
    labels_per_year <- 1
  }

  if (isTRUE(facetted)) {
    labels_per_year <- labels_per_year / 2
  }

  # Use pretty to generate similar breaks
  approx_breaks <- pretty(
    limits,
    n = labels_per_year * years_in_data # n is approximate number of breaks
  )

  return(approx_breaks)
}

january_breaks <- function(limits) {
  approx_breaks <- calc_breaks(
    limits = limits,
    facetted = FALSE
  )

  # compare month of all labels with january
  label_months <- lubridate::month(approx_breaks)

  earliest_month <- min(label_months)

  # calc difference in months from january
  difference_in_months <- earliest_month - 1

  new_breaks <- approx_breaks %m+% months(difference_in_months)

  return(new_breaks)
}

january_breaks_facetted <- function(limits) {
  approx_breaks <- calc_breaks(
    limits = limits,
    facetted = TRUE
  )

  # compare month of all labels with january
  label_months <- lubridate::month(approx_breaks)

  earliest_month <- min(label_months)

  # calc difference in months from january
  difference_in_months <- earliest_month - 1

  new_breaks <- approx_breaks %m+% months(difference_in_months)

  return(new_breaks)
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

  if (!is.null(facet)) {
    nearest_idx <- nearest_idx |>
      filter(
        .data$months_waited_id == facet
      )
  } else {
    nearest_idx <- nearest_idx |>
      mutate(
        months_waited_id = NA_real_
      )
  }

  if (nrow(nearest_idx) > 1) {
    # this will only occur for the first period after the observed data finishes
    # because it is artificially extended so it is displayed better on the
    # charts
    nearest_idx <- nearest_idx |>
      filter(.data$period_type == "Projected")
  }

  return(nearest_idx)
}

#' @importFrom utils head tail
performance_text <- function(p_target_data) {
  p_target_data <- p_target_data |>
    mutate(
      Target_date = format(.data$Target_date, "%b %Y"),
      final_text = paste0(
        .data$Target_percentage,
        "% (",
        .data$Target_date,
        ")"
      )
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
#' @import ggplot2
#' @importFrom rlang .data
linear_tooltip <- function() {
  dplyr::tibble(
    period = 1:10,
    value = c(rep(1, 5), 2:6)
  ) |>
    ggplot(
      aes(x = .data$period, y = .data$value)
    ) +
    geom_line() +
    geom_vline(
      xintercept = 5,
      linetype = "dashed"
    ) +
    geom_text(
      data = dplyr::tibble(
        period = 6,
        value = Inf,
        label = "Forecast"
      ),
      aes(
        label = .data$label
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
        colour = "#FAE100"
      ),
      plot.background = element_rect(
        fill = "#FAE100",
        colour = "#FAE100"
      )
    ) +
    ylim(0, 8)
}

#' @import ggplot2
#' @importFrom rlang .data
uniform_tooltip <- function() {
  dplyr::tibble(
    period = c(1:5, 5:10),
    value = c(rep(1, 5), rep(4, 6))
  ) |>
    ggplot(
      aes(x = .data$period, y = .data$value)
    ) +
    geom_line() +
    geom_vline(
      xintercept = 5,
      linetype = "dashed"
    ) +
    geom_text(
      data = dplyr::tibble(
        period = 6,
        value = Inf,
        label = "Forecast"
      ),
      aes(
        label = .data$label
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
        colour = "#FAE100"
      ),
      plot.background = element_rect(
        fill = "#FAE100",
        colour = "#FAE100"
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

skew_tooltip <- function() {
  shiny::HTML(
    paste0(
      "A skew of 1 causes the profile of treatment capacity rates across the number of months waiting to be unchanged from the calibration period.<br><br>",
      "A skew of greater than 1 will increase the treatment capacity rate for the longer waiters, and decrease the treatment capacity rate for the shorter waiters.<br><br>",
      "A skew of less than 1 will decrease the treatment capacity rate for the longer waiters, and increase the treatment capacity rate for the shorter waiters.<br><br>",
      "All skew values leave the treatment capacity rates for individuals waiting less than 1 month unchanged from the treatment capacity rate calculated from the calibration period."
    )
  )
}

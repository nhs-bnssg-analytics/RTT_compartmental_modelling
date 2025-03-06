
#' @description Create plot(s) for the selected feature
#'
#' @param data table of model results;
#' @param list of model selections;
#' @param title string; title string for chart
#'
#' @importFrom dplyr filter distinct rename left_join join_by
#' @importFrom ggtext element_markdown
#' @importFrom scales percent comma
#' @import ggplot2
#' @return plot of selected values
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
                        p_facet = F) {
  p <- ggplot2::ggplot() +
    geom_line(
      data = dplyr::filter(data, period_type == "Observed"),
      aes(
        x = period,
        y = p_var,
        group = 1
      ), colour = "black",
      show.legend = T
    ) +
    geom_line(
      data = dplyr::filter(data, period_type == "Projected"),
      aes(
        x = period,
        y = p_var,
        group = 2
      ),
      colour = "blue",
      linetype = "dashed"
    ) +
    theme_minimal() +
    ylab(NULL) +
    xlab(NULL)


  #<span style='color:blue'></span>
if (p_scenario == "Estimate performance (from capacity inputs)") {
  p <- p +
    labs(
      title = paste0("<b>", p_trust, "</b> : ", p_speciality),
      subtitle = paste0(
        "<span style='color:black'>**Observed**</span> <span style='color:#425563'>and </span> <span style='color:blue'>**projected** </span><span style='color:#425563'>", p_chart, ": ", format(min(dat$period), "%b %Y"), "-", format(max(dat$period), "%b %Y"),
        "<br>Performance based on a ", p_cap_change_type, " capacity change of ", p_cap_change, "% with a utilisation skew factor of ", p_cap_skew,
        "<br>Referrals ", p_referrals_change_type, "ly adjusted by ", p_referrals_percent_change, "% </span>"
      ),
      caption = paste0("Data taken from www.england.nhs.uk/statistics/statisical-work-areas/rtt-waiting-times - ", format(Sys.Date(), "%d/%m/%Y"))
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
        "<span style='color:black'>**Observed**</span><span style='color:#425563'> and </span><span style='color:blue'>**projected** </span><span style='color:#425563'>", p_chart, ": ", format(min(dat$period), "%b %Y"), "-", format(max(dat$period), "%b %Y"),
        "<br>Optimised capacity with a utilisation skew factor of ", p_cap_skew, " to achieve a target of ", p_target_performance, " by ", p_target_date,
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
        scale_y_continuous(labels = scales::percent)
    } else {
      p <- p +
        scale_y_continuous(labels = scales::comma)
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

  p
}

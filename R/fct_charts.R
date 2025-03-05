
#' @description Create plot(s) for the selected feature
#'
#' @param data table of model results;
#' @param list of model selections;
#' @param title string; title string for chart
#'
#' @importFrom dplyr filter distinct rename left_join join_by
#' @importFrom ggtext element_markdown
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
                        p_perc) {
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
    ylab("Total patients waiting") +
    xlab(NULL) +
    scale_x_date(breaks = "3 month", minor_breaks = "1 month", date_labels = "%b %y")

    if (p_scenario == "Estimate performance (from capacity inputs)") {
      p <- p +
        labs(
          title = paste0("**",p_trust, " : **", p_speciality),
          subtitle =  paste0("<span style='color:black'>**Observed**</span> and<span style='color:blue'>**projected**</span> ", p_chart, ": ", format(min(dat$period), "%b %Y"), "-", format(max(dat$period), "%b %Y"),
                             "Performance based on a ", p_cap_change_type, " capacity change of ", p_cap_change, "% with a utilisation skew factor of ", p_cap_skew,
                             "Referrals ", p_referrals_change_type,"ly adjusted by ", p_referrals_percent_change,"% "),
          caption = paste0("*Data taken from www.england.nhs.uk/statistics/statisical-work-areas/rtt-waiting-times - ", format(Sys.Date(), "%d/%m/%Y"),"*")
        )+
        theme(plot.title = ggtext::element_markdown(),
              plot.subtitle = ggtext::element_markdown(),
              plot.caption = ggtext::element_markdown())} else {
          p <- p +
            labs(
              title = paste0(p_trust, " : ", p_speciality),
              subtitle = paste0( "Observed and projected ", p_chart, ": ", format(min(dat$period), "%b %Y"), "-", format(max(dat$period), "%b %Y"),
                                 "\nOptimised capacity with a utilisation skew factor of ", p_cap_skew, " to achieve a target of ", p_target_performance, " by ", p_target_date,
                                 "\nReferrals ", p_referrals_change_type,"ly adjusted by ", p_referrals_percent_change,"%"),
              caption = paste0("Data taken from www.england.nhs.uk/statistics/statisical-work-areas/rtt-waiting-times - ", format(Sys.Date(), "%d/%m/%Y"))
            )
        }

    if (p_perc == T ) {
      p <- p +
        scale_y_continuous(labels = scales::percent)
    }

  p
}

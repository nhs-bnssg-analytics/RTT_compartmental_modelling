#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib navset_tab nav_panel nav_spacer nav_menu nav_item
#'   page_fillable page_navbar accordion accordion_panel layout_columns
#' @importFrom utils packageVersion
#' @noRd
app_ui <- function(request) {
  github_shiny <- tags$a(
    shiny::icon("github"),
    "Shiny",
    href = "https://github.com/nhsengland/RTT_compartmental_modelling",
    target = "_blank"
  )

  github_RTT_package <- tags$a(
    shiny::icon("github"),
    "R package",
    href = "https://github.com/nhs-bnssg-analytics/NHSRtt",
    target = "_blank"
  )

  github_analysis <- tags$a(
    shiny::icon("github"),
    "Analysis",
    href = "https://github.com/nhs-bnssg-analytics/sw_waiting_times",
    target = "_blank"
  )

  email <- tags$a(
    shiny::icon("envelope"),
    "Contact us",
    href = "mailto:sebastian.fox3@nhs.net?subject=RTT planning tool",
    target = "_blank"
  )

  theme_selection <- "litera"

  acknowledgements <- HTML(
    paste(
      "The RTT Planner was a collaboration driven by the SW Decision Support Network",
      "",
      "Many thanks to the collaborators:",
      "Sebastian Fox",
      "Simon Wellesley-Miller",
      "Richard Wood",
      "Richard Blackwell",
      "Claire Rudler",
      "Nick Cooper",
      "",
      "And input from Devon, Dorset, Gloucestershire and BNSSG ICSs along with NHSE SW",
      sep = "<br>"
    )
  )


  page_fillable(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    theme = bs_theme(bootswatch = theme_selection),
    # Your application UI logic

    page_navbar(
      title = HTML(
        paste0(
        'RTT Planner ',
        '<span style="font-size: 0.7rem;">(v',
        packageVersion("RTTshiny"),
        ') </span>'
        )
      ),
      bg = "#0072CE",
      theme = bs_theme(bootswatch = theme_selection),
      nav_panel(
        title = "How to use the tool",
        value = "tab_intro",
        mod_01_introduction_ui("01_introduction_1")
      ),
      nav_panel(
        title = "Scenario planner",
        value = "tab_configuration",
        mod_02_planner_ui("02_planner_1")
      ),
      nav_panel(
        title = "Results",
        value = "tab_results",
        mod_03_results_ui("03_results_1")
      ),
      nav_panel(
        title = "Downloads",
        value = "tab_download",
        mod_04_downloads_ui("04_downloads_1")
      ),
      nav_panel(
        title = "Definitions",
        value = "tab_definitions",
        mod_05_definitions_ui("05_definitions_1")
      ),
      nav_panel(
        title = "How tos",
        value = "tab_how_tos",
        mod_06_how_tos_ui("06_how_tos_1")
      ),
      nav_spacer(),
      nav_menu(
        title = "Links",
        nav_item(github_shiny),
        nav_item(github_RTT_package),
        nav_item(github_analysis),
        nav_item(email),
        align = "right"
      ),
      footer = card_footer(
        layout_columns(
          col_widths = c(10, 2),
          p(
            HTML(
              paste0(
              "Please raise any issues on <a href='https://github.com/nhs-bnssg-analytics/RTT_compartmental_modelling/issues'>https://github.com/nhs-bnssg-analytics/RTT_compartmental_modelling/issues</a> or send feedback to
             <a href='mailto:sebastian.fox3@nhs.net?subject=RTT planning tool (version ",
              packageVersion("RTTshiny"),
              ")'>sebastian.fox3@nhs.net</a>"
              )
            ),
            class = "text-center text-muted"
          ),
          accordion(
            accordion_panel(
              title = "Acknowledgements",
              acknowledgements

            ),
            open = FALSE
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "RTTshiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

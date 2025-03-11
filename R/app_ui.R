#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib navset_tab nav_panel nav_spacer nav_menu nav_item
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

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    h1(
      paste0(
        "RTT planner (version ",
        packageVersion("RTTshiny"),
        ")"
      )
    ),
    p("A tool to help the NHS plan to reduce waiting times"),
    tagList(
      navset_tab(
        nav_panel(
          title = "How to use the tool",
          mod_01_introduction_ui("01_introduction_1")
        ),
        nav_panel(
          title = "Scenario planner",
          mod_02_planner_ui("02_planner_1")
        ),
        nav_panel(
          title = "Results",
          mod_03_results_ui("03_results_1")
        ),
        nav_spacer(),
        nav_menu(
          title = "Links",
          nav_item(github_shiny),
          nav_item(github_RTT_package),
          nav_item(github_analysis),
          nav_item(email)
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

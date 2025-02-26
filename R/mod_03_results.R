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
        # plotOutput(
        #   ns("wl_size"),
        #   click = "plot_click"
        # )
      ),
      nav_panel(title = "Two", p("Second tab content.")),
      nav_panel(title = "Three", p("Second tab content.")),
      nav_panel(title = "Four", p("Second tab content.")),
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

    ## Create plots here
    # output$wl_size <- renderPlot({
  #   ggplot(r$waiting_list, aes(wt, mpg)) + geom_point()
  # }, res = 96)

  })
}

## To be copied in the UI
# mod_03_results_ui("03_results_1")

## To be copied in the server
# mod_03_results_server("03_results_1")

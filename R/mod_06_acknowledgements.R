#' 06_stories UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS h2 p div HTML
#' @importFrom bslib page_fluid card card_header card_body layout_column_wrap
mod_06_acknowledgements_ui <- function(id){
  ns <- NS(id)

  timeline_data <- data.frame(
    id = 1:5,
    content = c(
      "Single-stock RTT research",
      "Multi-stock RTT collaboration",
      "ICS collaboration",
      "ICS and NHS England development",
      "First release"
    ),
    date = c("From December 2019", "Winter 2024", "November 2024", "December 2024 to March 2025", "May 2025"),
    description = c(
      paste0(
        "First RTT model developed in NHS BNSSG ICB, and <a href='https://pmc.ncbi.nlm.nih.gov/articles/PMC8143633/'>research paper published.</a>",
        "<br><br>",
        "Research continues along with more associated publications."
      ),
      "NHS BNSSG ICB, in collaboration with Lancaster University develop multi-stock model using public NHS RTT statistics at England geography. At the time of writing, the associated research paper is in review (URL to be shared once published).",
      "NHS BNSSG ICB and NHS Devon ICB, who have also been working on stock-and-flow models, agree to develop common RTT model to reduce multiplication. This is facilitated by the South West Decision Support Network's 'At Scale Analytics' workstream.",
      "Collaboration expands to include NHS England South West team and NHS Gloucestershire ICB, and the development of the interactive online tool begins.",
      "In collaboration with the Midlands Strategy Unit, the first release of the online tool occurs."
    ),
    color = c("#330072", "#AE2573", "#8A1538", "#ED8B00", "#FFB81C")
  )

  generate_cards <- function(data) {
    cards_list <- list()

    for (i in 1:nrow(data)) {
      cards_list[[i]] <- card(
        style = paste0("border-radius: 5px; border-left: 4px solid ", data$color[i], ";"),
        card_header(data$content[i]),
        card_body(
          div(
            class = "timeline-item mb-4",
            style = if(i < nrow(data)) {
              "border-left: 2px solid #dee2e6; padding-left: 20px; position: relative;"
            } else {
              "padding-left: 20px; position: relative;"
            },

            # Date marker
            div(
              class = "timeline-marker",
              style = paste0("position: absolute; left: -10px; background-color: ", data$color[i], "; width: 20px; height: 20px; border-radius: 50%;"),
              ""
            ),

            # Event content
            div(
              p(
                shiny::HTML(
                  data$description[i]
                ),
                class = "mb-0 mt-2",
                style = "font-size: 0.8em;"
              ),
              p(
                data$date[i],
                class = "text-muted",
                style = "font-size: 0.8em; text-align: right;"
              )
            )
          )
        )#,
        # height = "200px"
      )
    }

    return(cards_list)
  }

  page_fluid(
    h2("Timeline of RTT Planner", class = "text-left"),
    p("The story of how this tool unfolded", class = "text-left text-muted"),
    do.call(
      layout_column_wrap,
      c(
        list(
          width = 1 / nrow(timeline_data),
          gap = "10px"
        ),
        generate_cards(timeline_data)
      )
    ),
    card(
      card_header(
        "Specific acknowledgements"
      ),
      card_body(
        HTML(
          paste(
            "The RTT Planner was a collaboration driven by the SW Decision Support Network.",
            "",
            "Many thanks to the collaborators:",
            "Sebastian Fox",
            "Simon Wellesley-Miller",
            "Richard Wood",
            "Richard Blackwell",
            "Claire Rudler",
            "Nick Cooper",
            "",
            "And input from Devon, Dorset, Gloucestershire and BNSSG ICSs along with NHSE SW.",
            sep = "<br>"
          )
        )
      )
    )
  )
}

#' 06_stories Server Functions
#'
#' @noRd
mod_06_acknowledgements_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_06_acknowledgements_ui("06_acknowledgements_1")

## To be copied in the server
# mod_06_acknowledgements_server("06_acknowledgements_1")

#' 07_stories UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS h2 p div
#' @importFrom bslib page_fluid card card_header card_body
mod_07_stories_ui <- function(id){
  ns <- NS(id)

  timeline_data <- data.frame(
    id = 1:6,
    content = c(
      "The Beginning",
      "First Major Challenge",
      "An Unexpected Turn",
      "The Discovery",
      "Moment of Truth",
      "Resolution"
    ),
    date = c("January 1, 2023", "March 15, 2023", "May 20, 2023", "June 25, 2023", "August 10, 2023", "September 15, 2023"),
    description = c(
      "Our journey began when...",
      "We faced our first obstacle when...",
      "Everything changed unexpectedly when...",
      "We made a breakthrough discovery that...",
      "The pivotal moment arrived when...",
      "Finally, everything came together as..."
    ),
    color = c("#8a2be2", "#d9534f", "#f0ad4e", "#5bc0de", "#5cb85c", "#428bca")
  )

  page_fluid(
    card(
      card_header(
        h2("Our Journey: A Timeline", class = "text-center"),
        p("The story of how it all unfolded", class = "text-center text-muted")
      ),
      card_body(
        # Text-based timeline
        lapply(1:nrow(timeline_data), function(i) {
          event <- timeline_data[i, ]
          div(
            class = "timeline-item mb-4",
            style = if(i < nrow(timeline_data)) "border-left: 2px solid #dee2e6; padding-left: 20px; position: relative;" else "padding-left: 20px; position: relative;",

            # Date marker
            div(
              class = "timeline-marker",
              style = paste0("position: absolute; left: -10px; background-color: ", event$color, "; width: 20px; height: 20px; border-radius: 50%;"),
              ""
            ),

            # Event content
            div(
              class = "timeline-content p-3",
              style = paste0("border-radius: 5px; background-color: #f8f9fa; border-left: 4px solid ", event$color, ";"),

              div(
                class = "d-flex justify-content-between",
                h4(event$content, class = "mb-2", style = paste0("color: ", event$color, ";")),
                span(event$date, class = "text-muted")
              ),
              p(event$description, class = "mb-0 mt-2")
            )
          )
        })
      ),
      min_height = "450px",
      full_screen = TRUE
    )
  )
}

#' 07_stories Server Functions
#'
#' @noRd
mod_07_stories_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_07_stories_ui("07_stories_1")

## To be copied in the server
# mod_07_stories_server("07_stories_1")

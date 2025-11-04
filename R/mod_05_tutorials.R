#' 05_how_tos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_05_tutorials_ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    card(
      card_body(
        h5("Background"),
        layout_column_wrap(
          width = 1 / 5,
          min_height = "300px",
          card(
            height = "400px",
            width = "200px",
            fill = TRUE,
            card_header("Modelling concepts"),

            tags$iframe(
              src = "https://www.youtube.com/embed/fjLtrm3kEuw?si=e5Q5HiQNHByWiQ6y",
              width = "100%",
              height = "100%",
              frameborder = "0",
              allowfullscreen = TRUE,
              title = "Modelling concepts"
            )
          ),
          card(
            height = "400px",
            width = "200px",
            fill = TRUE,
            card_header("Navigating the tool"),

            tags$iframe(
              src = "https://www.youtube.com/embed/URX5bez1ACg?si=CpHhjQSGCbQWW4-O",
              width = "100%",
              height = "100%",
              frameborder = "0",
              allowfullscreen = TRUE,
              title = "Navigating the tool"
            )
          ),
          card(
            height = "400px",
            width = "200px",
            fill = TRUE,
            card_header("Steady state"),

            tags$iframe(
              src = "https://www.youtube.com/embed/39syGvz9CIY?si=JHGxR2GVBc93DFBz",
              width = "100%",
              height = "100%",
              frameborder = "0",
              allowfullscreen = TRUE,
              title = "Steady state"
            )
          )
        ),
        h5("Data inputs"),
        layout_column_wrap(
          width = 1 / 5,
          min_height = "300px",
          card(
            height = "400px",
            width = "200px",
            fill = TRUE,
            card_header("Downloading data and calibrating the model"),

            tags$iframe(
              src = "https://www.youtube.com/embed/Nh2sav9lv3g?si=BGxzdKResbM1sNut",
              width = "100%",
              height = "100%",
              frameborder = "0",
              allowfullscreen = TRUE,
              title = "Downloading data and calibrating the model"
            )
          ),
          card(
            height = "400px",
            width = "200px",
            fill = TRUE,
            card_header("Upload your own data"),

            tags$iframe(
              src = "https://www.youtube.com/embed/63TEDGRNNQs?si=kCFv7An2H0EPJMTM",
              width = "100%",
              height = "100%",
              frameborder = "0",
              allowfullscreen = TRUE,
              title = "Upload your own data"
            )
          ),
          card(
            height = "400px",
            width = "200px",
            fill = TRUE,
            card_header("Model uncertainty"),

            tags$iframe(
              src = "https://www.youtube.com/embed/6-xCCP4bRr4?si=MglNr2cxqAEASsYn",
              width = "100%",
              height = "100%",
              frameborder = "0",
              allowfullscreen = TRUE,
              title = "Model uncertainty"
            )
          )
        ),
        h5("Configuring the projections"),
        layout_column_wrap(
          width = 1 / 5,
          min_height = "300px",
          card(
            height = "400px",
            width = "200px",
            fill = TRUE,
            card_header("Predicting performance from treatment capacity"),

            tags$iframe(
              src = "https://www.youtube.com/embed/fK4Zf-UA_e0?si=92cwn_ceMFV0y3cz",
              width = "100%",
              height = "100%",
              frameborder = "0",
              allowfullscreen = TRUE,
              title = "Predicting performance from treatment capacity"
            )
          ),
          card(
            height = "400px",
            width = "200px",
            fill = TRUE,
            card_header("Optimising treatment capacity from performance"),

            tags$iframe(
              src = "https://www.youtube.com/embed/2-EG7FTwLyo?si=VunHhJdv_s7t4xJ2",
              width = "100%",
              height = "100%",
              frameborder = "0",
              allowfullscreen = TRUE,
              title = "Optimising treatment capacity from performance"
            )
          ),
          card(
            height = "400px",
            width = "200px",
            fill = TRUE,
            card_header("Skew factor"),

            tags$iframe(
              src = "https://www.youtube.com/embed/TjJ_kKewQx0?si=o6D4Lcpa60pkXdyN",
              width = "100%",
              height = "100%",
              frameborder = "0",
              allowfullscreen = TRUE,
              title = "Skew factor"
            )
          ),
          card(
            height = "400px",
            width = "200px",
            fill = TRUE,
            card_header("Short term measures"),

            tags$iframe(
              src = "https://www.youtube.com/embed/rENSXbJDOt8?si=EpZBo5tckuThkVik",
              width = "100%",
              height = "100%",
              frameborder = "0",
              allowfullscreen = TRUE,
              title = "Short term measures"
            )
          )
        ),
        h5("Results"),
        layout_column_wrap(
          width = 1 / 5,
          min_height = "300px",
          card(
            height = "400px",
            width = "200px",
            fill = TRUE,
            card_header("Results and download"),

            tags$iframe(
              src = "https://www.youtube.com/embed/HumupDCv_Oo?si=7kYeHI0lJ-RMAPKg",
              width = "100%",
              height = "100%",
              frameborder = "0",
              allowfullscreen = TRUE,
              title = "Results and download"
            )
          ),
          card(
            height = "400px",
            width = "200px",
            fill = TRUE,
            card_header("Shortfall chart"),

            tags$iframe(
              src = "https://www.youtube.com/embed/d96IsheJsQ4?si=n6eHyZWYl-HBBUWJ",
              width = "100%",
              height = "100%",
              frameborder = "0",
              allowfullscreen = TRUE,
              title = "Shortfall chart"
            )
          )
        )
      )
    )
  )
}

#' 05_how_tos Server Functions
#'
#' @noRd
mod_05_tutorials_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_05_tutorials_ui("05_tutorials_1")

## To be copied in the server
# mod_05_tutorials_server("05_tutorials_1")

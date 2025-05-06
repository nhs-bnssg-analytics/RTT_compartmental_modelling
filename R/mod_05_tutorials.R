#' 05_how_tos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_05_tutorials_ui <- function(id){
  ns <- NS(id)
  page_fluid(
    layout_column_wrap(
      width = 1 / 5,
      card(
        height = "400px",
        width = "200px",
        card_header("Modelling concepts"),

        tags$iframe(
          src = "https://nhs-my.sharepoint.com/personal/sebastian_fox3_nhs_net/_layouts/15/embed.aspx?UniqueId=20b4a879-0270-4606-86af-3d290fa6e793&embed=%7B%22ust%22%3Atrue%2C%22hv%22%3A%22CopyEmbedCode%22%7D&referrer=StreamWebApp&referrerScenario=EmbedDialog.Create",
          width = "100%",
          height = "100%",
          frameborder = "0",
          allowfullscreen = TRUE
        )
        # HTML(
        #   '<iframe src="https://nhs-my.sharepoint.com/personal/sebastian_fox3_nhs_net/_layouts/15/embed.aspx?UniqueId=20b4a879-0270-4606-86af-3d290fa6e793&embed=%7B%22ust%22%3Atrue%2C%22hv%22%3A%22CopyEmbedCode%22%7D&referrer=StreamWebApp&referrerScenario=EmbedDialog.Create" width="640" height="360" frameborder="0" scrolling="no" allowfullscreen title="Recording-20250506_162049.webm"></iframe>'
        # )
      )
    )
  )
}

#' 05_how_tos Server Functions
#'
#' @noRd
mod_05_tutorials_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_05_tutorials_ui("05_tutorials_1")

## To be copied in the server
# mod_05_tutorials_server("05_tutorials_1")

#' 04_downloads UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_04_downloads_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("download_button"))
  )
}

#' 04_downloads Server Functions
#' @importFrom shiny downloadButton downloadHandler renderUI showModal modalDialog
#' @noRd
mod_04_downloads_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$download_button <- renderUI({
      if (!is.null(r$waiting_list)) {
        if (!requireNamespace("flextable", quietly = TRUE)) {
          showModal(
            modalDialog(
              title = "flextable missing",
              "The 'flextable' package is required to enable reporting functionality. If you would like this, please exit the app and run 'install.packages('flextable')'.",
              easyClose = TRUE,
              footer = NULL
            )
          )
        } else {
          downloadButton(
            ns("report_btn"),
            "Generate report",
            style = "width:25%;"
          )
        }
      }
    })


    output$report_btn <- downloadHandler(
      filename <-  "Trust planning report.docx",
      content = function(file) {

        tempReport <- file.path(tempdir(), "skeleton.Rmd")

        file.copy(
          system.file("rmarkdown", "templates", "scenario-report", "skeleton", "skeleton.Rmd", package = "RTTshiny"),
          tempReport,
          overwrite = TRUE
        )
        params <- list(
          waiting_list = r$waiting_list,
          trust = r$chart_specification$trust,
          specialty = r$chart_specification$specialty
        )
        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )

      }
    )

  })
}

## To be copied in the UI
# mod_04_downloads_ui("04_downloads_1")

## To be copied in the server
# mod_04_downloads_server("04_downloads_1")

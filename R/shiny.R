#' Visual summaries for pedestrian data of a month window
#'
#' @return A shiny app.
#' @export
#'
#' @examples
#' \dontrun{
#'   shine_melb()
#' }
shine_melb <- function() {
  shiny <- requireNamespace("shiny", quietly = TRUE)
  plotly <- requireNamespace("plotly", quietly = TRUE)
  if (!shiny && !plotly) {
    stop(
      "Packages shiny & plotly required for shine_melb()", ".\n",
      "Please install and try again.", call. = FALSE
    )
  }

  ui <- shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 5,
        shiny::dateRangeInput(
          "date_rng", "Date range:",
          start = Sys.Date() - 6L,
          end = Sys.Date() - 1L
        ),
        shiny::selectizeInput(
          "sensor", "Sensor:",
          choices = sensor,
          multiple = TRUE
        ),
        shiny::downloadButton("csv_dl", "Download CSV")
      )
    )
  )

  server <- function(input, output, session) {
    # Update date range allowing for a month window
    shiny::observe({
      date <- input$date_rng[2]
      shiny::updateDateRangeInput(
        session, "date_rng",
        min = date - 30L,
        max = date
      )
    })

    all_df <- shiny::reactive({
      walk_melb(from = input$date_rng[1], to = input$date_rng[2])
    })
    ped_df <- shiny::reactive({
      if (is.null(input$sensor)) {
        all_df()
      } else {
        dplyr::filter(all_df(), Sensor %in% input$sensor)
      }
    })

    output$csv_dl <- shiny::downloadHandler(
      filename = function() {
        paste0("pedestian-", Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(ped_df(), file)
      }
    )
  }

  shiny::shinyApp(ui, server)
}

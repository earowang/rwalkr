#' A simple shiny app for pedestrian data of a given month window
#'
#' @return A shiny app.
#' @export
#'
#' @examples
#' \dontrun{
#'   shine_melb()
#' }
shine_melb <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Packages shiny required for shine_melb()", ".\n",
      "Please install and try again.", call. = FALSE
    )
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop(
      "Packages plotly required for shine_melb()", ".\n",
      "Please install and try again.", call. = FALSE
    )
  }
  `%>%` <- plotly::`%>%`

  ui <- shiny::fluidPage(
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::dateRangeInput(
          "date_rng", "Date range:",
          start = Sys.Date() - 7L,
          end = Sys.Date() - 1L
        ),
        shiny::selectizeInput(
          "sensor_txt", "Sensor:",
          choices = sensor$sensor,
          multiple = TRUE
        ),
        shiny::downloadButton("csv_dl", "Download CSV")
      ),
      shiny::column(
        width = 7,
        plotly::plotlyOutput("overlay", height = 320),
        shiny::hr(),
        plotly::plotlyOutput("marker", height = 480)
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
      walk_melb(
        from = input$date_rng[1], to = input$date_rng[2],
        session = FALSE
      )
    })
    ped_df <- shiny::reactive({
      if (is.null(input$sensor_txt)) {
        all_df()
      } else {
        dplyr::filter(all_df(), Sensor %in% input$sensor_txt)
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

    output$overlay <- plotly::renderPlotly({
      tsplot <- ped_df() %>%
        dplyr::filter(!is.na(Count)) %>%
        dplyr::group_by(Sensor) %>%
        plotly::plot_ly(
          x = ~ Date_Time, y = ~ Count,
          hoverinfo = "text",
          text = ~ paste(
            "Sensor: ", Sensor,
            "<br> Date Time: ", Date_Time
          )
        ) %>%
        plotly::add_lines(alpha = 0.8)
      plotly::layout(
        tsplot, title = "Time series plot",
        xaxis = list(title = "Date Time"), yaxis = list(title = "Count")
      )
    })

    output$marker <- plotly::renderPlotly({
      na_df <- ped_df() %>%
        dplyr::left_join(sensor, by = c("Sensor" = "sensor")) %>%
        dplyr::mutate(NA_ind = is.na(Count))
      miss_marker <- plotly::plot_ly(
        na_df, hoverinfo = "text",
        text = ~ paste(
          "Sensor:", Sensor,
          "<br> Date Time: ", Date_Time,
          "<br> Missing: ", NA_ind
        )
      ) %>%
        plotly::add_markers(
          x = ~ Date_Time, y = ~ abbr, color = ~ NA_ind,
          colors = c("#1b9e77", "#7570b3")
        )
      plotly::layout(
        miss_marker, title = "Missing value indicator",
        showlegend = FALSE,
        xaxis = list(title = "Date Time"), yaxis = list(title = "")
      )
    })
  }

  shiny::shinyApp(ui, server)
}

mod_hm14_ui <- function(id) {
  ns <- NS(id)
  hm14_tier <- filter_series(guide_rbnz, column = c("Split", "Names", "Grouping"), apply_filters = list(Graph = "hm14")) |>
    dplyr::group_by(Split) |>
    dplyr::summarise(
      value = list(
        as.list(stats::setNames(Names, Grouping))
      ),
      .groups = "drop"
    ) |>
    tibble::deframe()

  insert_inputs <- tagList(
    selectInput(
      ns("hm14_tier"),
      "Expectation Type",
      choices = hm14_tier,
      selected = c(
        "Annual GDP growth - 1 year out",
        "Annual CPI growth - 1 year out",
        "Perception of monetary conditions (net) - 1 year out"
      ),
      multiple = TRUE
    )
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hm14_server <- function(id, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    hm14_data <- reactive({
      load_data("hm14")
    })
    hm14_plot <- reactive({
      req(enabled())
      x <- input$hm14_tier
      if (length(x) >= 4) {x <- x[1:4]}
      if (length(x) == 0) {
        x <- c(
          "Annual GDP growth - 1 year out",
          "Annual CPI growth - 1 year out",
          "Perception of monetary conditions (net) - 1 year out"
        )
      }
      generic_plotly(
        data = hm14_data(),
        titles = c("Perceptions and Expectations", ""),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm14"), Names = x)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ hm14_plot() })
  })
}

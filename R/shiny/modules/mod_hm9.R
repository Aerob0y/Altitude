mod_hm9_ui <- function(id) {
  ns <- NS(id)
  hm9_tier <- filter_series(guide_rbnz, column = c("Split", "Names", "Grouping"), apply_filters = list(Graph = "hm9")) |>
    dplyr::group_by(Split) |>
    dplyr::summarise(
      value = list(
        as.list(stats::setNames(Names, Grouping))
      ),
      .groups = "drop"
    ) |>
    tibble::deframe()
  insert_inputs <- tagList(
    selectInput(ns("hm9_tier1"), "Metric (max 4)",  choices = hm9_tier, selected = c("Labour force participation rate        - % s.a.", "Labour cost index (LCI) - y/y%"), multiple = TRUE)
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hm9_server <- function(id, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    hm9_data <- reactive({
      load_data("hm9")
    })
    hm9_plot <- reactive({
      req(enabled())
      x <- input$hm9_tier1
      if (length(x) >= 4) {x <- x[1:4]}
      if (length(x) == 0) {x <- c("Labour force participation rate       - % s.a.", "Labour cost index (LCI) - y/y%")}
      generic_plotly(
        data = hm9_data(),
        titles = c("National Saving", paste("RBNZ:", short_title(x))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm9"), Names = x)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ hm9_plot() })
  })
}

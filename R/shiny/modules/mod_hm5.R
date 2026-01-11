mod_hm5_ui <- function(id) {
  ns <- NS(id)
  hm5_tier <- filter_series(
    guide_rbnz,
    column = c("Split", "Names"),
    apply_filters = list(Graph = c("hm5"))
  ) |>
    dplyr::group_by(Split) |>
    dplyr::summarise(value = list(Names), .groups = "drop") |>
    tibble::deframe()
  insert_inputs <- tagList(
    selectInput(
      ns("hm5_names"),
      "GDP Measures (Max 4)",
      choices = hm5_tier,
      selected = c("GDP - Expenditure (Real $m s.a.)", "GDP - Expenditure (Real y/y%)"),
      multiple = TRUE
    )
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hm5_server <- function(id, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    hm5_data <- reactive({
      load_data("hm5")
    })
    hm5_plot <- reactive({
      req(enabled())
      valid_inputs <- input$hm5_names |> unlist(use.names = FALSE) |> unique()
      valid_inputs <- valid_inputs[seq_len(min(length(valid_inputs), 4))]
      generic_plotly(
        data = hm5_data(),
        titles = c("GDP", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm5"), Names = valid_inputs)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ hm5_plot() })
  })
}

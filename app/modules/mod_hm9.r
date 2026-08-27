mod_hm9_ui <- function(id) {
  if (checks$ui_module) print("hm9_ui loaded")
  ns <- NS(id)

  hm9_tier <- filter_series(
    guide_rbnz,
    column = c("Class_1", "Name", "Class_2"),
    apply_filters = list(Data = c("hm9"))
  ) |>
    dplyr::group_by(Class_1) |>
    dplyr::summarise(
      value = list(as.list(stats::setNames(Name, Class_2))),
      .groups = "drop"
    ) |>
    tibble::deframe()

  insert_inputs <- tagList(
    selectInput(
      ns("hm9_tier1"),
      "Metric (max 4)",
      choices = hm9_tier,
      selected = c("Labour force participation rate        - % s.a.", "Labour cost index (LCI) - y/y%"),
      multiple = TRUE
    )
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm9")
}

mod_hm9_server <- function(id, selected_tab, activate_on, plot_function = x_plotly) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hm9_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hm9_server loaded")
      load_data("hm9")
    }, ignoreInit = FALSE)

    observeEvent(input$hm9_tier1, {
      req(input$hm9_tier1)
      if (length(input$hm9_tier1) > 4) {
        updateSelectInput(session, "hm9_tier1", selected = input$hm9_tier1[1:4])
      }
    }, ignoreInit = TRUE)

    hm9_plot <- reactive({
      req(enabled())
      x <- input$hm9_tier1
      if (length(x) == 0) x <- c("Labour force participation rate       - % s.a.", "Labour cost index (LCI) - y/y%")
      x <- x[seq_len(min(length(x), 4))]

      plot_function(
        data = hm9_data(),
        titles = c("Labour market", paste("RBNZ:", short_title(x))),
        series = filter_series(guide_rbnz, apply_filters = list(Data = c("hm9"), Name = x)),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm9_plot())
  })
}

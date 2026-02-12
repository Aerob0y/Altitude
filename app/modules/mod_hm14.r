mod_hm14_ui <- function(id) {
  if (checks$ui_module) print("hm14_ui loaded")
  ns <- NS(id)

  hm14_tier <- filter_series(
    guide_rbnz,
    column = c("Class_1", "Name", "Class_2"),
    apply_filters = list(Data = c("hm14"))
  ) |>
    dplyr::group_by(Class_1) |>
    dplyr::summarise(
      value = list(as.list(stats::setNames(Name, Class_2))),
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

mod_hm14_server <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hm14_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hm14_server loaded")
      load_data("hm14")
    }, ignoreInit = FALSE)

    observeEvent(input$hm14_tier, {
      req(input$hm14_tier)
      if (length(input$hm14_tier) > 4) {
        updateSelectInput(session, "hm14_tier", selected = input$hm14_tier[1:4])
      }
    }, ignoreInit = TRUE)

    hm14_plot <- reactive({
      req(enabled())
      x <- input$hm14_tier
      if (length(x) == 0) {
        x <- c(
          "Annual GDP growth - 1 year out",
          "Annual CPI growth - 1 year out",
          "Perception of monetary conditions (net) - 1 year out"
        )
      }
      x <- x[seq_len(min(length(x), 4))]

      x_plotly(
        data = hm14_data(),
        titles = c("Perceptions and Expectations", ""),
        series = filter_series(guide_rbnz, apply_filters = list(Data = c("hm14"), Name = x)),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm14_plot())
  })
}

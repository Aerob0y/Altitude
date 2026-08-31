mod_hm14_ui <- function(id, series_guide = guide_rbnz) {
  if (checks$ui_module) print("hm14_ui loaded")
  ns <- NS(id)

  hm14_tier <- filter_series(
    series_guide,
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

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm14")
}

mod_hm14_server <- function(id, selected_tab, activate_on, plot_function = x_plotly, series_guide = guide_rbnz) {
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

      plot_function(
        data = hm14_data(),
        titles = c("Perceptions and Expectations", ""),
        series = filter_series(series_guide, apply_filters = list(Data = c("hm14"), Name = x)),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm14_plot())
  })
}
mod_hm14_ui_update <- function(id) {
  # 1. Namespace
  ns <- NS(id)

  # 2. Series selection ----
  hm14_tier <- filter_series(
    guide,
    column = c("Category_1", "ColumnName", "Category_1"),
    apply_filters = list(Dataset = c("hm14"))
  ) |>
    dplyr::group_by(Category_1) |>
    dplyr::summarise(
      value = list(as.list(stats::setNames(ColumnName, Category_2))),
      .groups = "drop"
    ) |>
    tibble::deframe()

  # 3. Create inputs ----
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
  # 4. Create UI ----
  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm14")
}

mod_hm14_server_update <- function(id, selected_tab, activate_on) {
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

      plot_function(
        data = hm14_data(),
        titles = c("Perceptions and Expectations", ""),
        series = filter_series(series_guide, apply_filters = list(Data = c("hm14"), Name = x)),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm14_plot())
  })
}

mod_hs32_ui <- function(id, series_guide = guide_rbnz) {
  if (checks$ui_module) print("hs32_ui loaded")
  ns <- NS(id)

  hs32_name <- filter_series(
    series_guide,
    column = "Name",
    apply_filters = list(Data = c("hs32"))
  )$Name
  hs32_name <- hs32_name[!is.na(hs32_name)]

  insert_inputs <- tagList(
    selectInput(
      ns("hs32_name"),
      "Series (max 6)",
      choices = hs32_name,
      selected = hs32_name[seq_len(min(3, length(hs32_name)))],
      multiple = TRUE
    )
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hs32")
}

mod_hs32_server <- function(id, selected_tab, activate_on, plot_function = x_plotly, series_guide = guide_rbnz) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hs32_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hs32_server loaded")
      load_data("hs32")
    }, ignoreInit = FALSE)

    observeEvent(input$hs32_name, {
      req(input$hs32_name)
      if (length(input$hs32_name) > 6) {
        updateSelectInput(session, "hs32_name", selected = input$hs32_name[1:6])
      }
    }, ignoreInit = TRUE)

    hs32_plot <- reactive({
      req(enabled())
      x <- input$hs32_name
      x <- x[seq_len(min(length(x), 6))]

      plot_function(
        data = hs32_data(),
        titles = c("HS32", paste("RBNZ:", short_title(x))),
        series = filter_series(series_guide, apply_filters = list(Data = c("hs32"), Name = x)),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hs32_plot())
  })
}

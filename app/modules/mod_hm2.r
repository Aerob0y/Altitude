mod_hm2_ui <- function(id) {
  if (checks$ui_module) print("hm2_ui loaded")
  ns <- NS(id)

  # Choices (force to character vector)
  hm2_class_2 <- filter_series(
    guide_rbnz,
    column = "Class_2",
    apply_filters = list(Data = c("hm2"))
  )$Class_2

  hm2_class_2 <- hm2_class_2[!is.na(hm2_class_2)]

  insert_inputs <- tagList(
    selectInput(
      ns("hm2_class_2"),
      "Consumption Type (vs)",
      choices = hm2_class_2,
      selected = if (length(hm2_class_2) > 0) hm2_class_2[1] else NULL,
      multiple = FALSE
    ),
    selectInput(
      ns("hm2_class_2_2"),
      NULL,
      choices = c("-", hm2_class_2),
      selected = "-",
      multiple = FALSE
    )
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm2")
}

mod_hm2_server <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    # Load data only when hm2 tab is active
    hm2_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hm2_server loaded")
      load_data("hm2")
    }, ignoreInit = FALSE)

    observeEvent(input$hm2_class_2, {
      req(input$hm2_class_2)
      if (!is.null(input$hm2_class_2_2) && input$hm2_class_2_2 == input$hm2_class_2) {
        updateSelectInput(session, "hm2_class_2_2", selected = "-")
      }
    }, ignoreInit = TRUE)

    hm2_plot <- reactive({
      req(enabled())
      d <- hm2_data()

      # Combine and clean selections
      valid_inputs <- unique(c(input$hm2_class_2, input$hm2_class_2_2))
      valid_inputs <- setdiff(valid_inputs, "-")
      valid_inputs <- valid_inputs[!is.na(valid_inputs)]

      t <- if (checks$sourcenames) "Consumption - HM2" else "Consumption"

      x_plotly(
        data = d,
        titles = c(t, paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(
          guide_rbnz,
          apply_filters = list(Data = c("hm2"), Class_2 = valid_inputs)
        ),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm2_plot())
  })
}

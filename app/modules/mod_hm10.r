mod_hm10_ui <- function(id) {
  if (checks$ui_module) print("hm10_ui loaded")
  ns <- NS(id)

  hm10_class_2 <- filter_series(
    guide_rbnz,
    column = "Class_2",
    apply_filters = list(Data = c("hm10"))
  )$Class_2
  hm10_class_2 <- hm10_class_2[!is.na(hm10_class_2)]

  insert_inputs <- tagList(
    selectInput(ns("hm10_class_2_1"), "Metric", choices = hm10_class_2, selected = hm10_class_2[1], multiple = FALSE),
    selectInput(ns("hm10_class_2_2"), NULL, choices = c("-", hm10_class_2), selected = hm10_class_2[2], multiple = FALSE)
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm10")
}

mod_hm10_server <- function(id, selected_tab, activate_on, plot_function = x_plotly) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hm10_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hm10_server loaded")
      load_data("hm10")
    }, ignoreInit = FALSE)

    # Prevent duplicates (same behaviour you wanted for hm2)
    observeEvent(input$hm10_class_2_1, {
      req(input$hm10_class_2_1)
      if (!is.null(input$hm10_class_2_2) && input$hm10_class_2_2 == input$hm10_class_2_1) {
        updateSelectInput(session, "hm10_class_2_2", selected = "-")
      }
    }, ignoreInit = TRUE)

    hm10_plot <- reactive({
      req(enabled())
      valid_inputs <- unique(c(input$hm10_class_2_1, input$hm10_class_2_2)) |> setdiff("-")

      plot_function(
        data = hm10_data(),
        titles = c("Housing", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(guide_rbnz, apply_filters = list(Data = c("hm10"), Class_2 = valid_inputs)),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm10_plot())
  })
}

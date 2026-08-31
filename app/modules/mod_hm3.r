mod_hm3_ui <- function(id) {
  if (checks$ui_module) print("hm3_ui loaded")
  ns <- NS(id)

  # Choices (force to character vector)
  hm3_class_1 <- filter_series(
    guide_rbnz,
    column = "Class_1",
    apply_filters = list(Data = c("hm3"))
  )$Class_1

  hm3_class_1 <- hm3_class_1[!is.na(hm3_class_1)]

  insert_inputs <- tagList(
    selectInput(
      ns("hm3_class_1"),
      "Investment Type (vs)",
      choices = hm3_class_1,
      selected = if (length(hm3_class_1) > 0) hm3_class_1[1] else NULL,
      multiple = FALSE
    ),
    selectInput(
      ns("hm3_class_1_2"),
      NULL,
      choices = c("-", hm3_class_1),
      selected = if (length(hm3_class_1) >= 4) hm3_class_1[4] else "-",
      multiple = FALSE
    )
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm3")
}

mod_hm3_server <- function(id, selected_tab, activate_on, plot_function = x_plotly) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    # Load data only when hm3 tab is active
    hm3_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hm3_server loaded")
      load_data("hm3")
    }, ignoreInit = FALSE)

    # Prevent duplicates in the compare selector
    observeEvent(input$hm3_class_1, {
      req(input$hm3_class_1)
      if (!is.null(input$hm3_class_1_2) && input$hm3_class_1_2 == input$hm3_class_1) {
        updateSelectInput(session, "hm3_class_1_2", selected = "-")
      }
    }, ignoreInit = TRUE)

    hm3_plot <- reactive({
      req(enabled())
      d <- hm3_data()

      # Combine and clean selections
      valid_inputs <- unique(c(input$hm3_class_1, input$hm3_class_1_2))
      valid_inputs <- setdiff(valid_inputs, "-")
      valid_inputs <- valid_inputs[!is.na(valid_inputs)]

      t <- if (checks$sourcenames) "Investment - HM3" else "Investment"

      plot_function(
        data = d,
        titles = c(t, paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(
          guide_rbnz,
          apply_filters = list(Data = c("hm3"), Class_1 = valid_inputs)
        ),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm3_plot())
  })
}


mod_hm3_ui_update <- function(id) {
  # 1. Namespace
  ns <- NS(id)

  # 2. Series selection ----
  hm3_class_1 <- filter_series_unlist(
    guide,
    column = "Category_1",
    apply_filters = list(Dataset = c("hm3"))
  )

  hm3_class_1 <- hm3_class_1[!is.na(hm3_class_1)]

  # 3. Create inputs ----
  insert_inputs <- tagList(
    selectInput(
      ns("hm3_class_1"),
      "Investment Type (vs)",
      choices = hm3_class_1,
      selected = if (length(hm3_class_1) > 0) hm3_class_1[1] else NULL,
      multiple = FALSE
    ),
    selectInput(
      ns("hm3_class_1_2"),
      NULL,
      choices = c("-", hm3_class_1),
      selected = if (length(hm3_class_1) >= 4) hm3_class_1[4] else "-",
      multiple = FALSE
    )
  )
  # 4. Create UI ----
  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm3")
}

mod_hm3_server_update <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    # Load data only when hm3 tab is active
    hm3_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hm3_server loaded")
      load_data("hm3")
    }, ignoreInit = FALSE)

    # Prevent duplicates in the compare selector
    observeEvent(input$hm3_class_1, {
      req(input$hm3_class_1)
      if (!is.null(input$hm3_class_1_2) && input$hm3_class_1_2 == input$hm3_class_1) {
        updateSelectInput(session, "hm3_class_1_2", selected = "-")
      }
    }, ignoreInit = TRUE)

    hm3_plot <- reactive({
      req(enabled())
      d <- hm3_data()

      # Combine and clean selections
      valid_inputs <- unique(c(input$hm3_class_1, input$hm3_class_1_2))
      valid_inputs <- setdiff(valid_inputs, "-")
      valid_inputs <- valid_inputs[!is.na(valid_inputs)]

      t <- if (checks$sourcenames) "Investment - HM3" else "Investment"

      standard_plot(
        data = d,
        titles = c(t, paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(
          guide,
          apply_filters = list(Dataset = c("hm3"), Category_1 = valid_inputs)
        ),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm3_plot())
  })
}

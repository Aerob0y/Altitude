mod_hm4_ui <- function(id, series_guide = guide_rbnz) {
  if (checks$ui_module) print("hm4_ui loaded")
  ns <- NS(id)

  hm4_class_1 <- filter_series(
    series_guide,
    column = "Class_1",
    apply_filters = list(Data = c("hm4"))
  )$Class_1
  hm4_class_1 <- hm4_class_1[!is.na(hm4_class_1)]

  hm4_dim <- filter_series(
    series_guide,
    column = "Dim",
    apply_filters = list(Data = c("hm4"))
  )$Dim
  hm4_dim <- hm4_dim[!is.na(hm4_dim)]

  insert_inputs <- tagList(
    selectInput(
      ns("hm4_dim"),
      "Dimension",
      choices = hm4_dim,
      selected = if (length(hm4_dim) > 0) hm4_dim[1] else NULL,
      multiple = FALSE
    ),
    selectInput(
      ns("hm4_class_1"),
      "Trade Group",
      choices = hm4_class_1,
      selected = if (length(hm4_class_1) > 0) hm4_class_1[1] else NULL,
      multiple = FALSE
    )
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm4")
}

mod_hm4_server <- function(id, selected_tab, activate_on, plot_function = x_plotly, series_guide = guide_rbnz) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hm4_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hm4_server loaded")
      load_data("hm4")
    }, ignoreInit = FALSE)

    hm4_plot <- reactive({
      req(enabled())
      d <- hm4_data()

      t <- if (checks$sourcenames) "Domestic Trade - HM4" else "Domestic Trade"

      plot_function(
        data = d,
        titles = c(t, paste("RBNZ:", short_title(input$hm4_class_1))),
        series = filter_series(
          series_guide,
          apply_filters = list(
            Data = c("hm4"),
            Class_1 = input$hm4_class_1,
            Dim = input$hm4_dim
          )
        ),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm4_plot())
  })
}

mod_hm4_ui_update <- function(id, series_guide = guide_rbnz) {
  # 1. Namespace
  ns <- NS(id)

  # 2. Series selection ----
  hm4_class_1 <- filter_series_unlist(
    guide,
    column = "Category_1",
    apply_filters = list(Dataset = c("hm4"))
  )
  hm4_class_1 <- hm4_class_1[!is.na(hm4_class_1)]

  hm4_dim <- filter_series_unlist(
    guide,
    column = "Dim_Group",
    apply_filters = list(Dataset = c("hm4"))
  )
  hm4_dim <- hm4_dim[!is.na(hm4_dim)]

  # 3. Create inputs ----
  insert_inputs <- tagList(
    selectInput(
      ns("hm4_dim"),
      "Dimension",
      choices = hm4_dim,
      selected = if (length(hm4_dim) > 0) hm4_dim[1] else NULL,
      multiple = FALSE
    ),
    selectInput(
      ns("hm4_class_1"),
      "Trade Group",
      choices = hm4_class_1,
      selected = if (length(hm4_class_1) > 0) hm4_class_1[1] else NULL,
      multiple = FALSE
    )
  )
  # 4. Create UI ----
  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm4")
}

mod_hm4_server_update <- function(id, selected_tab, activate_on, plot_function = x_plotly, series_guide = guide_rbnz) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hm4_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hm4_server loaded")
      load_data("hm4")
    }, ignoreInit = FALSE)

    hm4_plot <- reactive({
      req(enabled())
      d <- hm4_data()

      t <- if (checks$sourcenames) "Domestic Trade - HM4" else "Domestic Trade"

      plot_function(
        data = d,
        titles = c(t, paste("RBNZ:", short_title(input$hm4_class_1))),
        series = filter_series(
          series_guide,
          apply_filters = list(
            Data = c("hm4"),
            Class_1 = input$hm4_class_1,
            Dim = input$hm4_dim
          )
        ),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm4_plot())
  })
}

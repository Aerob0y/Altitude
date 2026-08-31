mod_hm6_ui <- function(id, series_guide = guide_rbnz) {
  if (checks$ui_module) print("hm6_ui loaded")
  ns <- NS(id)

  hm6_name <- filter_series(
    series_guide,
    column = "Name",
    apply_filters = list(Data = c("hm6"))
  )$Name
  hm6_name <- hm6_name[!is.na(hm6_name)]

  insert_inputs <- tagList(
    selectInput(ns("hm6_name"), "Saving Types", choices = hm6_name, selected = hm6_name, multiple = TRUE)
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm6")
}

mod_hm6_server <- function(id, selected_tab, activate_on, plot_function = x_plotly, series_guide = guide_rbnz) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hm6_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hm6_server loaded")
      load_data("hm6")
    }, ignoreInit = FALSE)

    hm6_plot <- reactive({
      req(enabled())
      plot_function(
        data = hm6_data(),
        titles = c("National Saving", paste("RBNZ:", short_title(input$hm6_name))),
        series = filter_series(series_guide, apply_filters = list(Data = c("hm6"), Name = input$hm6_name)),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm6_plot())
  })
}

mod_hm6_ui_update <- function(id) {
  # 1. Namespace
  ns <- NS(id)

  # 2. Series selection ----
  hm6_name <- filter_series(
    guide,
    column = "Name",
    apply_filters = list(Data = c("hm6"))
  )$Name
  hm6_name <- hm6_name[!is.na(hm6_name)]

  # 3. Create inputs ----
  insert_inputs <- tagList(
    selectInput(ns("hm6_name"), "Saving Types", choices = hm6_name, selected = hm6_name, multiple = TRUE)
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm6")
}

mod_hm6_server_update <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hm6_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hm6_server loaded")
      load_data("hm6")
    }, ignoreInit = FALSE)

    hm6_plot <- reactive({
      req(enabled())
      plot_function(
        data = hm6_data(),
        titles = c("National Saving", paste("RBNZ:", short_title(input$hm6_name))),
        series = filter_series(series_guide, apply_filters = list(Data = c("hm6"), Name = input$hm6_name)),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm6_plot())
  })
}

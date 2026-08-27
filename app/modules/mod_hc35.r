mod_hc35_ui <- function(id, series_guide = guide_rbnz) {
  if (checks$ui_module) print("hc35_ui loaded")
  ns <- NS(id)

  hc35_group <- filter_series(
    series_guide,
    column = "Class_1",
    apply_filters = list(Data = c("hc35"))
  )$Class_1
  hc35_group <- hc35_group[!is.na(hc35_group)]

  hc35_split <- filter_series(
    series_guide,
    column = "Class_2",
    apply_filters = list(Data = c("hc35"))
  )$Class_2
  hc35_split <- hc35_split[!is.na(hc35_split)]

  insert_inputs <- tagList(
    selectInput(ns("hc35_group"), "Lending Group", choices = hc35_group, selected = hc35_group[1], multiple = FALSE),
    checkboxGroupInput(ns("hc35_split"), "Lending", choices = hc35_split, selected = hc35_split[c(1, 3, 4)])
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hc35")
}

mod_hc35_server <- function(id, selected_tab, activate_on, plot_function = x_plotly, series_guide = guide_rbnz) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hc35_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hc35_server loaded")
      load_data("hc35")
    }, ignoreInit = FALSE)

    hc35_plot <- reactive({
      req(enabled())
      d <- hc35_data()
      print("X")
      plot_function(
        data = d,
        titles = c(
          "Residential mortgage loan reconciliation",
          paste("RBNZ:", short_title(unique(c(input$hc35_group, input$hc35_split))))
        ),
        series = filter_series(
          series_guide,
          apply_filters = list(
            Data = c("hc35"),
            Class_2 = input$hc35_split,
            Class_1 = input$hc35_group
          )
        ),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hc35_plot())
  })
}

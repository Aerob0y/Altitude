mod_hm1_ui <- (function(id) {
  ns <- NS(id)
  hm1_input <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm1")))
  hm1_metric <- filter_series(guide_rbnz, column = "Dim", apply_filters = list(Graph = c("hm1")))
  insert_inputs <- tagList(
    selectInput(ns("hm1_metric"), "Metric",  choices = hm1_metric, selected = "y/y%", multiple = FALSE),
    checkboxGroupInput(ns("hm1_input"), "Price Index (5 Max)",  choices = hm1_input, selected = hm1_input[c(1, 4, 5, 6)])
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
})

mod_hm1_server <- (function(id, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    hm1_data <- reactive({
      load_data("hm1")
    })
    hm1_plot <- reactive({
      print("hm1_plot reactive")
      generic_plotly(
        data = hm1_data(),
        titles = c("Prices", paste("RBNZ:", input$hm1_metric)),
        series = filter_series(
          guide_rbnz,
          apply_filters = list(
            Graph = c("hm1"),
            Split = input$hm1_input[seq_len(min(length(input$hm1_input), 5))],
            Dim = input$hm1_metric
          )
        ),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ hm1_plot() })
  })
})

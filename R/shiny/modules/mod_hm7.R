mod_hm7_ui <- function(id) {
  ns <- NS(id)
  hm7_group <- filter_series(guide_rbnz, column = "Grouping", apply_filters = list(Graph = c("hm7")))
  insert_inputs <- tagList(
    selectInput(ns("hm7_group"), "Balance Group",  choices = hm7_group, selected = hm7_group[1], multiple = FALSE)
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hm7_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hm7_plot <- reactive({
      generic_plotly(
        data = load_data("hm7"),
        titles = c("Balance of Payments", paste("RBNZ:", input$hm7_group)),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm7"), Grouping = input$hm7_group)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ hm7_plot() })
  })
}

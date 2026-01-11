mod_fuel_ui <- function(id) {
  ns <- NS(id)
  insert_inputs <- tagList(
    selectInput(ns("fuel_unit"), "Fuel Unit",  choices = c("USD per Barrel", "NZD per Barrel"), selected = "USD per Barrel", multiple = FALSE)
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_fuel_server <- function(id, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    fuel_data <- reactive({
      load_data("fuel")
    })
    fuel_plot <- reactive({
      req(enabled())
      generic_plotly(
        data = fuel_data(),
        titles = c("Jet A1 Fuel Prices per Barrel", paste("In", input$fuel_unit)),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("fuel"), Split = input$fuel_unit)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ fuel_plot() })
  })
}

mod_fuel_ui <- function(id) {
  if (checks$ui_module) print("fuel_ui loaded")
  ns <- NS(id)

  insert_inputs <- tagList(
    selectInput(ns("fuel_unit"), "Fuel Unit",  choices = c("USD per Barrel", "NZD per Barrel"), selected = "USD per Barrel", multiple = FALSE)
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_fuel_server <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    fuel_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("fuel_server loaded")
      load_data("fuel")
    }, ignoreInit = FALSE)

    fuel_plot <- reactive({
      req(enabled())
      x <- input$fuel_unit

      x_plotly(
        data = fuel_data(),
        titles = c("Fuel", paste("RBNZ:", short_title(x))),
        series = filter_series(guide_rbnz, apply_filters = list(Data = c("fuel"), Class_2 = x)),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(fuel_plot())
  })
}

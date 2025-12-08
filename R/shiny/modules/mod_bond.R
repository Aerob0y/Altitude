mod_bond_ui <- function(id) {
  ns <- NS(id)
  bond_locations <- load_data("bond") %>% dplyr::pull(Location) %>% unique()
  bond_metrics <- filter_series(guide_rbnz, apply_filters = list(Graph = c("bond"), Names = NULL))
  insert_inputs <- tagList(
    selectInput(ns("bond_location"), "Location", choices = bond_locations, selected = bond_locations, multiple = TRUE),
    selectInput(ns("bond_metric"), "Metric", choices = bond_metrics, selected = bond_metrics[1], multiple = FALSE)
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_bond_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    bond_plot <- reactive({
      data <- load_data("bond")
      data <- data %>% dplyr::filter(Location %in% input$bond_location)
      plot_long(
        data = data,
        titles = c("Bond Prices", ""),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("bond"), Names = input$bond_metric)),
        k = "Date",
        split = "Location"
      )
    })
    output$plot <- renderPlotly({ bond_plot() })
  })
}

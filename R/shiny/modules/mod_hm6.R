mod_hm6_ui <- function(id) {
  ns <- NS(id)
  hm6_names <- filter_series(guide_rbnz, column = "Names", apply_filters = list(Graph = c("hm6")))
  insert_inputs <- tagList(
    selectInput(ns("hm6_names"), "Saving Types",  choices = hm6_names, selected = hm6_names, multiple = TRUE)
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hm6_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hm6_plot <- reactive({
      generic_plotly(
        data = load_data("hm6"),
        titles = c("National Saving", paste("RBNZ:", short_title(input$hm6_names))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm6"), Names = input$hm6_names)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ hm6_plot() })
  })
}

mod_hm10_ui <- function(id) {
  ns <- NS(id)
  hm10_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm10")))
  insert_inputs <- tagList(
    selectInput(ns("hm10_split_1"), "Metric",  choices = hm10_split, selected = hm10_split[1], multiple = FALSE),
    selectInput(ns("hm10_split_2"), NULL,  choices = c("-", hm10_split), selected = hm10_split[2], multiple = FALSE)
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hm10_server <- function(id, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    hm10_data <- reactive({
      load_data("hm10")
    })
    hm10_plot <- reactive({
      req(enabled())
      valid_inputs <- unique(c(input$hm10_split_1, input$hm10_split_2)) |> setdiff("-")
      generic_plotly(
        data = hm10_data(),
        titles = c("Housing", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm10"), Split = valid_inputs)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ hm10_plot() })
  })
}

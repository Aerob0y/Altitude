mod_hm8_ui <- function(id) {
  ns <- NS(id)
  hm8_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm8")))
  hm8_grouping <- filter_series(guide_rbnz, column = "Grouping", apply_filters = list(Graph = c("hm8")))
  insert_inputs <- tagList(
    selectInput(ns("hm8_grouping"), "Metric",  choices = hm8_grouping, selected = hm8_grouping[1], multiple = FALSE),
    checkboxGroupInput(ns("hm8_split"), "Trade Type",  choices = hm8_split, selected = hm8_split)
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hm8_server <- function(id, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    hm8_data <- reactive({
      load_data("hm8")
    })
    hm8_plot <- reactive({
      req(enabled())
      generic_plotly(
        data = hm8_data(),
        titles = c("Overseas Trade", paste("RBNZ:", short_title(unique(c(input$hm8_group1, input$hm8_group2))))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm8"), Split = input$hm8_split, Grouping = input$hm8_grouping)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ hm8_plot() })
  })
}

mod_hm3_ui <- function(id) {
  ns <- NS(id)
  hm3_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm3")))
  insert_inputs <- tagList(
    selectInput(ns("hm3_split"), "Investment Type",  choices = hm3_split, selected = hm3_split[1], multiple = FALSE),
    selectInput(ns("hm3_split2"), "Second Investment Type",  choices = c("-", hm3_split), selected = hm3_split[4], multiple = FALSE)
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hm3_server <- function(id, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    hm3_data <- reactive({
      load_data("hm3")
    })
    hm3_plot <- reactive({
      req(enabled())
      valid_inputs <- unique(c(input$hm3_split, input$hm3_split2)) |> setdiff("-")
      generic_plotly(
        data = hm3_data(),
        titles = c("Investment", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm3"), Split = valid_inputs)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ hm3_plot() })
  })
}

mod_hm2_ui <- function(id) {
  ns <- NS(id)
  hm2_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm2")))
  insert_inputs <- tagList(
    selectInput(ns("hm2_split"), "Consumption Type (vs)",  choices = hm2_split, selected = "General government consumption expenditure (GDP) NZDm(r) s.a.", multiple = FALSE),
    selectInput(ns("hm2_split2"), NULL,  choices = c("-", hm2_split), selected = "Private consumption expenditure (GDP) %(r) s.a.", multiple = FALSE)
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hm2_server <- function(id, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    hm2_data <- reactive({
      load_data("hm2")
    })
    hm2_plot <- reactive({
      req(enabled())
      valid_inputs <- unique(c(input$hm2_split, input$hm2_split2)) |> setdiff("-")
      generic_plotly(
        data = hm2_data(),
        titles = c("Consumption", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm2"), Split = valid_inputs)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ hm2_plot() })
  })
}

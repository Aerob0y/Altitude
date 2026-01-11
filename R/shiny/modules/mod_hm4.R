mod_hm4_ui <- function(id) {
  ns <- NS(id)
  hm4_group <- filter_series(guide_rbnz, column = "Grouping", apply_filters = list(Graph = c("hm4")))
  hm4_dim <- filter_series(guide_rbnz, column = "Dim", apply_filters = list(Graph = c("hm4")))
  insert_inputs <- tagList(
    selectInput(ns("hm4_dim"), "Dimension",  choices = hm4_dim, selected = hm4_dim[1], multiple = FALSE),
    selectInput(ns("hm4_group"), "Trade Group",  choices = hm4_group, selected = hm4_group[1], multiple = FALSE)
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hm4_server <- function(id, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    hm4_data <- reactive({
      load_data("hm4")
    })
    hm4_plot <- reactive({
      req(enabled())
      generic_plotly(
        data = hm4_data(),
        titles = c("Domestic Trade", paste("RBNZ:", short_title(input$hm4_group))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm4"), Grouping = input$hm4_group)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ hm4_plot() })
  })
}

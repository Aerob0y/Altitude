mod_hc35_ui <- function(id) {
  ns <- NS(id)
  hc35_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hc35")))
  hc35_group <- filter_series(guide_rbnz, column = "Group", apply_filters = list(Graph = c("hc35")))
  insert_inputs <- tagList(
    selectInput(ns("hc35_group"), "Lending Group",  choices = hc35_group, selected = hc35_group[1], multiple = FALSE),
    checkboxGroupInput(ns("hc35_split"), "Lending",  choices = hc35_split, selected = hc35_split[c(1, 3, 4)])
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hc35_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hc35_plot <- reactive({
      generic_plotly(
        data = load_data("hc35"),
        titles = c("Residential mortgage loan reconciliation", paste("RBNZ:", short_title(unique(c(input$hc35_group, input$hc35_split))))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hc35"), Split = input$hc35_split, Group = input$hc35_group)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ hc35_plot() })
  })
}

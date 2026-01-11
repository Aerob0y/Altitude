filter_series(guide_rbnz, column = c("Split", "Names", "Grouping"), apply_filters = list(Graph = "hs32"))

mod_hm32_ui <- function(id) {
  ns <- NS(id)
  hs32_choice <- c("Total", "Loan Type", "Movement")
  hs32_grouping <- filter_series(guide_rbnz, column = c("Grouping"), apply_filters = list(Graph = "hs32"))
  hs32_series <- filter_series(guide_rbnz, column = c("Split"), apply_filters = list(Graph = "hs32"))
  hs32_series <- hs32_series[which(hs32_series != "Total")]

  insert_inputs <- tagList(
    selectInput(
      ns("hs32_choice"),
      "Split By",
      choices = hs32_choice,
      selected = hs32_choice,
      multiple = TRUE
    ),
    selectInput(
      ns("hs32_series"),
      "Expectation Type",
      choices = hs32_series,
      selected = hs32_series,
      multiple = TRUE
    ),
    selectInput(
      ns("hs32_grouping"),
      "Expectation Type",
      choices = hs32_grouping,
      selected = hs32_grouping,
      multiple = TRUE
    )
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}




mod_hm32_server <- function(id, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    hm32_data <- reactive({
      load_data("hs32")
    })
    hm32_plot <- reactive({
      req(enabled())
      x <- input$hs32_choice
      #data <- switch(x,
      #"Total" <-  {filter_series(guide_rbnz, apply_filters = list(Graph = c("hs32"), Names = x) },
      #"Loan Type" <- {filter_series(guide_rbnz, apply_filters = list(Graph = c("hs32"), Names = x)},
      #"Movement" <- {filter_series(guide_rbnz, apply_filters = list(Graph = c("hs32"), Names = x)}
      #)
      #}
      
      
      generic_plotly(
        data = hs32_data(),
        titles = c("Perceptions and Expectations", ""),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hs32"), Names = x)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({ hs32_plot() })
  })
}

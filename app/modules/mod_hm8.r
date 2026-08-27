mod_hm8_ui <- function(id) {
  if (checks$ui_module) print("hm8_ui loaded")
  ns <- NS(id)

  hm8_split <- filter_series(
    guide_rbnz,
    column = "Class_2",
    apply_filters = list(Data = c("hm8"))
  )$Class_2
  hm8_split <- hm8_split[!is.na(hm8_split)]

  hm8_grouping <- filter_series(
    guide_rbnz,
    column = "Class_1",
    apply_filters = list(Data = c("hm8"))
  )$Class_1
  hm8_grouping <- hm8_grouping[!is.na(hm8_grouping)]

  insert_inputs <- tagList(
    selectInput(ns("hm8_grouping"), "Metric", choices = hm8_grouping, selected = hm8_grouping[1], multiple = FALSE),
    checkboxGroupInput(ns("hm8_split"), "Trade Type", choices = hm8_split, selected = hm8_split)
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm8")
}

mod_hm8_server <- function(id, selected_tab, activate_on, plot_function = x_plotly) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hm8_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hm8_server loaded")
      load_data("hm8")
    }, ignoreInit = FALSE)

    hm8_plot <- reactive({
      req(enabled())
      valid_inputs <- unique(input$hm8_split)

      plot_function(
        data = hm8_data(),
        titles = c("Overseas Trade", paste("RBNZ:", short_title(unique(c(input$hm8_grouping, valid_inputs))))),
        series = filter_series(
          guide_rbnz,
          apply_filters = list(Data = c("hm8"), Class_2 = valid_inputs, Class_1 = input$hm8_grouping)
        ),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm8_plot())
  })
}

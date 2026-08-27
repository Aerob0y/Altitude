mod_hm7_ui <- function(id) {
  if (checks$ui_module) print("hm7_ui loaded")
  ns <- NS(id)

  hm7_group <- filter_series(
    guide_rbnz,
    column = "Class_1",
    apply_filters = list(Data = c("hm7"))
  )$Class_1
  hm7_group <- hm7_group[!is.na(hm7_group)]

  insert_inputs <- tagList(
    selectInput(ns("hm7_group"), "Balance Group", choices = hm7_group, selected = hm7_group[1], multiple = FALSE)
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm7")
}

mod_hm7_server <- function(id, selected_tab, activate_on, plot_function = x_plotly) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hm7_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hm7_server loaded")
      load_data("hm7")
    }, ignoreInit = FALSE)

    hm7_plot <- reactive({
      req(enabled())
      plot_function(
        data = hm7_data(),
        titles = c("Balance of Payments", paste("RBNZ:", input$hm7_group)),
        series = filter_series(guide_rbnz, apply_filters = list(Data = c("hm7"), Class_1 = input$hm7_group)),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm7_plot())
  })
}

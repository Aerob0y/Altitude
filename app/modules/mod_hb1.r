mod_hb1_ui <- function(id) {
  ns <- NS(id)
  class_1 <- filter_series_unlist(guide_rbnz, column = "Class_1", apply_filters = list(Data = c("hb1")))
  ui_single(
    id = ns("hb1"),
    tagList(
      selectInput(ns("Currency1_main"), "Currency", choices = class_1, selected = "NZD/USD"),
      selectInput(ns("Currency2_main"), "Secondary", choices = c("-", class_1), selected = "-"),
      checkboxInput(ns("bond_split"), "Split by location", value = FALSE),
      tags$div(class = "dl-compact dl-row", download_settings_ui(ns))
    ),
    p = ns("plot"),
    h = "600px",
    module = "hb1"
  )
}


mod_hb1_server <- function(id, selected_tab, activate_on, plot_function = x_plotly) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hb1_data <- eventReactive(enabled(), {
      req(enabled())
      load_data("hb1")
    }, ignoreInit = FALSE)

    dl <- mod_download_server("dl")

    hb1_plot <- reactive({
      req(enabled())
      valid_inputs <- unique(c(input$Currency1_main, input$Currency2_main)) |> setdiff("-")
      plot_function(
        data = hb1_data(),
        titles = c("Daily exchange rates and TWI", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(
          guide_rbnz,
          apply_filters = list(Data = "hb1", Class_1 = valid_inputs)
        ),
        k = "Date",
        years = 15,
        download   = dl$download(),
        clean_ui   = dl$clean_export()
      )
    })

    output$plot <- renderPlotly(hb1_plot())
  })
}

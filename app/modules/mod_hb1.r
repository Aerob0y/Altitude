mod_hb1_ui <- function(id, series_guide = guide_rbnz) {
  ns <- NS(id)
  class_1 <- filter_series_unlist(series_guide, column = "Class_1", apply_filters = list(Data = c("hb1")))
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


mod_hb1_server <- function(id, selected_tab, activate_on, plot_function = x_plotly, series_guide = guide_rbnz) {
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
          series_guide,
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


mod_hb1_ui_update <- function(id) {
  # 1. Namespace
  ns <- NS(id)

  # 2. Series selection ----
  currencies <- filter_series_unlist(guide, column = "Category_1", apply_filters = list(Dataset = c("hb1")))

  # 3. Create inputs ----
  insert_inputs <- tagList(
    selectInput(ns("Currency1_main"), "Currency", choices = currencies, selected = "NZD/USD"),
    selectInput(ns("Currency2_main"), "Secondary", choices = c("-", currencies), selected = "-"),
    tags$div(class = "dl-compact dl-row", download_settings_ui(ns))
  )

  # 4. Create UI ----
  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hb1_update")
}


mod_hb1_server_update <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {

    enabled <- reactive(identical(selected_tab(), activate_on))

    # --------------------------------------------------------------------------
    # Plot

    output$plot <- renderPlotly({
      req(enabled())
      req(input$Currency1_main)
      req(input$Currency2_main)
      valid_inputs <- unique(c(input$Currency1_main, input$Currency2_main)) |> setdiff("-")

      standard_plot(
        data =          load_data("hb1"),
        titles =        c("Daily exchange rates and TWI", paste("RBNZ:", short_title(valid_inputs))),
        series =        filter_series(guide, apply_filters = list(Dataset = "hb1", Category_1 = valid_inputs)),
        split_by =      NULL,
        split_columns = NULL,
        k = "Date"
      )
    })
  })
}

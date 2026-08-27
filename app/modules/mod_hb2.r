mod_hb2_ui <- function(id, series_guide = guide_rbnz) {
  head(series_guide)
  ns <- NS(id)
  hb2_tier <- filter_series(
    series_guide,
    column = c("Class_1", "Name"),
    apply_filters = list(Data = c("hb2"))
  ) |>
    dplyr::group_by(Class_1) |>
    dplyr::summarise(value = list(Name), .groups = "drop") |>
    tibble::deframe()

  insert_inputs <- tagList(
    selectInput(
      ns("hb2_tier"),
      "Interest Rate Tier",
      choices = hb2_tier,
      selected = c(hb2_tier$`Cash rate`, hb2_tier$`Swap rates close`[3]),
      multiple = TRUE
    ),
    tags$div(class = "dl-compact dl-row", download_settings_ui(ns))
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hb2")
}


mod_hb2_server <- function(id, selected_tab, activate_on, plot_function = x_plotly, series_guide = guide_rbnz) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))


    # Load data only when hb2 tab is active
    hb2_data <- eventReactive(enabled(), {
      req(enabled())
      load_data("hb2")
    }, ignoreInit = FALSE)





    hb2_plot <- reactive({
      req(enabled())
      d <- hb2_data()
      print("hb2_data loaded")
      print(head(d))
      print("series_guide")
      print(head(series_guide))
      valid_inputs <- input$hb2_tier |> unlist(use.names = FALSE) |> unique()
      if (length(valid_inputs) == 0) {
        valid_inputs <- c("Official Cash Rate (OCR)")
      }
      valid_inputs <- valid_inputs[seq_len(min(length(valid_inputs), 5))]
      plot_function(
        data = d,
        titles = c("Daily wholesale interest rates", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(series_guide, apply_filters = list(Data = c("hb2"), Name = valid_inputs)),
        k = "Date",
        years = 15
      )
    })
    output$plot <- renderPlotly({ hb2_plot() })
  })
}

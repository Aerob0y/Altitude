mod_hb2_ui <- function(id) {
  print("hb2_ui loaded")
  ns <- NS(id)
  hb2_tier <- filter_series(
    guide,
    column = c("Class_1", "Name"),
    apply_filters = list(Dataset = c("hb2"))
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

mod_hb2_ui_update <- function(id) {
  
  # 1. Namespace
  ns <- NS(id)

  # 2. Series selection ----
  tier <- filter_series(guide, column = c("Category_1", "ColumnName"), apply_filters = list(Dataset = c("hb2"))) %>%
    group_by(Category_1) |>
    summarise(value = list(ColumnName), .groups = "drop") |>
    deframe()

  # 3. Create inputs ----
  insert_inputs <- tagList(selectInput(ns("hb2_tier"),
      "Interest Rate Tier",
      choices = tier,
      selected = c(tier$`Cash rate`, tier$`Swap rates close`[3]),
      multiple = TRUE
    ),
    tags$div(class = "dl-compact dl-row", download_settings_ui(ns))
  )
  # 4. Create UI ----
  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hb2_update")
}


mod_hb2_server_update <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {

    # 1. Check if module is active
    enabled <- reactive(identical(selected_tab(), activate_on))

    # 2. Load data only when hb2 tab is active
    output$plot <- renderPlotly({
      req(enabled())
      valid_inputs <- input$hb2_tier |> unlist(use.names = FALSE) |> unique()
      if (length(valid_inputs) == 0) {valid_inputs <- c("Official Cash Rate (OCR)")}
      valid_inputs <- valid_inputs[seq_len(min(length(valid_inputs), 5))]

      standard_plot(
        data = load_data("hb2"),
        titles = c("Daily wholesale interest rates", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(guide, apply_filters = list(Dataset = c("hb2"), ColumnName = valid_inputs)),
        k = "Date",
        years = 15
      )
    })
  })
}

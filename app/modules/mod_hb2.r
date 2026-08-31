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
      selected = c(tier$`OCR`, tier$`Swap rates close`[1:4]),
      multiple = TRUE
    ),
    tags$div(class = "dl-compact dl-row", download_settings_ui(ns))
  )
  # 4. Create UI ----
  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hb2")
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

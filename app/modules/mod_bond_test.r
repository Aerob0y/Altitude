# ==============================================================================
# Bond UI ----------------------------------------------------------------------

mod_bond_ui_test <- function(id) {
  # 1. Namespace
  ns <- NS(id)

  # 2. Series selection ----
  locations <- load_data("bond_locations") %>% unique()
  tier <- filter_series(guide, column = c("Category_1", "ColumnName"), apply_filters = list(Dataset = "bond")) %>%
    group_by(Category_1) %>%
    summarise(value = list(ColumnName), .groups = "drop") %>%
    deframe() %>%
    lapply(\(x) stats::setNames(x, x))

  split <- c("-", filter_series(guide, column = "ColumnName", apply_filters = list(Dataset = "bond")))

  # 3. Create inputs ----
  insert_inputs <- tagList(
    selectInput(ns("tier"),         label = "Tier",               choices = tier,      selected = unname(tier$Rent[1:4]),   multiple = TRUE),
    selectInput(ns("location"),     label = "Location",           choices = locations, selected = locations[1],             multiple = TRUE),
    selectInput(ns("split"),        label = "Split by location",  choices = split,     selected = "-",                      multiple = FALSE),
    tags$div(class = "dl-compact dl-row", download_settings_ui(ns))
  )
  # 4. Create UI ----
  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "bond_test")
}




# ==============================================================================
# Bond Server ------------------------------------------------------------------

mod_bond_server_test <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    # enabled
    enabled <- reactive(identical(selected_tab(), activate_on))

    # Change series when tier changes
    bond_series <- reactive({
      req(enabled())
      req(input$tier)
      filter_series(guide, apply_filters = list(Dataset = "bond", ColumnName = input$tier))
    })


    # --------------------------------------------------------------------------
    # Plot

    output$plot <- renderPlotly({
      req(enabled())
      req(input$tier)
      req(input$location)

      standard_plot(
        data =          load_data("bond") %>% filter(Location %in% input$location),
        titles =        c("Bond", paste("RBNZ:", short_title(input$tier))),
        series =        bond_series(),
        split_by =      if (input$split != "-") "Location" else NULL,
        split_columns = if (input$split != "-") input$split else NULL,
        k = "Date"
      )
    })
  })
}

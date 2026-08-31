mod_bond_uix <- function(id) {
  if (checks$ui_module) print("bond_ui loaded")
  ns <- NS(id)

  # Get Metrics
  name <- filter_series(
    guide_rbnz,
    column = "Name",
    apply_filters = list(Data = c("bond"))
  ) %>%
    as.vector() %>%
    unlist() %>%
    unname()

  #Other data
  bond_locations <- load_data("bond_locations") %>% unique()

  # Get Inputs
  insert_inputs <- tagList(
    selectInput(ns("bond_metric"), "Metric",  choices = name, selected = name[1], multiple = FALSE),
    selectInput(ns("bond_location"), "Unit", choices = bond_locations, selected = bond_locations[1], multiple = TRUE),
    tags$div(class = "dl-compact dl-row", download_settings_ui(ns))
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "bond")
}

mod_bond_serverx <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    bond_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("bond_server loaded")
      load_data("bond")
    }, ignoreInit = FALSE)

    bond_plot <- reactive({
      req(enabled())
      x <- input$bond_unit

      x_plotly(
        data = filter(bond_data(), Location %in% input$bond_location),
        titles = c("Bond", paste("RBNZ:", short_title(x))),
        split = "Location",
        series = filter_series(guide_rbnz, apply_filters = list(Data = c("bond"), Name = input$bond_metric)),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(bond_plot())
  })
}
mod_hm5_ui <- function(id, series_guide = guide_rbnz) {
  if (checks$ui_module) print("hm5_ui loaded")
  ns <- NS(id)

  # Tiered choices: Class_1-style grouping via Split → Names
  hm5_tier <- filter_series(
    series_guide,
    column = c("Class_1", "Name"),
    apply_filters = list(Data = c("hm5"))
  ) |>
    dplyr::group_by(Class_1) |>
    dplyr::summarise(value = list(Name), .groups = "drop") |>
    tibble::deframe()

  insert_inputs <- tagList(
    selectInput(
      ns("hm5_name"),
      "GDP Measures (Max 4)",
      choices = hm5_tier,
      selected = c(
        "GDP - Expenditure (Real $m s.a.)",
        "GDP - Expenditure (Real y/y%)"
      ),
      multiple = TRUE
    )
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm5")
}

mod_hm5_server <- function(id, selected_tab, activate_on, plot_function = x_plotly, series_guide = guide_rbnz) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    # Load data only when hm5 tab is active
    hm5_data <- eventReactive(enabled(), {
      req(enabled())
      if (checks$ui_module) print("hm5_server loaded")
      load_data("hm5")
    }, ignoreInit = FALSE)

    # Enforce max 4 selections at UI level
    observeEvent(input$hm5_name, {
      req(input$hm5_name)
      if (length(input$hm5_name) > 4) {
        updateSelectInput(
          session,
          "hm5_name",
          selected = input$hm5_name[1:4]
        )
      }
    }, ignoreInit = TRUE)

    hm5_plot <- reactive({
      req(enabled())
      d <- hm5_data()

      valid_inputs <- input$hm5_name |>
        unlist(use.names = FALSE) |>
        unique()

      valid_inputs <- valid_inputs[seq_len(min(length(valid_inputs), 4))]

      plot_function(
        data = d,
        titles = c("GDP", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(
          series_guide,
          apply_filters = list(
            Data = c("hm5"),
            Name = valid_inputs
          )
        ),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm5_plot())
  })
}
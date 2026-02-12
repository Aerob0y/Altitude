mod_hb2_ui <- function(id) {
  # ID Setup
  if (checks$ui_module) {print(paste0("ui hb2"))}  # DEBUG
  ns <- NS(id)

  # Get Metrics
  hb2_tier <- filter_series(
    guide_rbnz,
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
    )
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hb2_server <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))


    # Load data only when hb2 tab is active
    hb2_data <- eventReactive(enabled(), {
      req(enabled())
      print("hb2_server loaded")
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
      x_plotly(
        data = d,
        titles = c("Daily wholesale interest rates", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(guide_rbnz, apply_filters = list(Data = c("hb2"), Name = valid_inputs)),
        k = "Date",
        years = 15
      )
    })
    output$plot <- renderPlotly({ hb2_plot() })
  })
}

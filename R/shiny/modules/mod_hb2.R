mod_hb2_ui <- function(id) {
  if (checks$ui_module) {print(paste0("ui hb2"))}  # DEBUG
  ns <- NS(id)
  hb2_tier <- filter_series(
    guide_rbnz,
    column = c("Group", "Names"),
    apply_filters = list(Graph = c("hb2"))
  ) |>
    dplyr::group_by(Group) |>
    dplyr::summarise(value = list(Names), .groups = "drop") |>
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

  ui_single2(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hb2_server <- function(id, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    hb2_data <- reactive({
      load_data("hb2")
    })

    hb2_plot <- reactive({
      req(enabled())
      valid_inputs <- input$hb2_tier |> unlist(use.names = FALSE) |> unique()
      if (length(valid_inputs) == 0) {
        valid_inputs <- c("Official Cash Rate (OCR)")
      }
      generic_plotly(
        data = hb2_data(),
        titles = c("Daily wholesale interest rates", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hb2"), Names = valid_inputs)),
        k = "Date",
        years = 15
      )
    })
    output$plot <- renderPlotly({ hb2_plot() })
  })
}

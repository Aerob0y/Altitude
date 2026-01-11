mod_hb1_ui <- function(id) {
  print("hb1_ui loaded")
  ns <- NS(id)
  hb1_split <- filter_series(
    guide_rbnz,
    column = "Split",
    apply_filters = list(Graph = c("hb1"))
  )

  insert_inputs <- tagList(
    selectInput(
      ns("Currency1_main"),
      "Currency",
      choices = hb1_split,
      selected = "NZD/USD",
      multiple = FALSE
    ),
    selectInput(
      ns("Currency2_main"),
      "Second Currency",
      choices = c("-", hb1_split),
      selected = "-",
      multiple = FALSE
    )
  )
  ui_single2(insert_inputs, p = ns("plot"), h = "600px")
}


mod_hb1_server <- function(id, selected_tab) {
  print("hb1_server loaded")

  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), "hb1"))

    observeEvent(selected_tab(), {
      cat("selected tab:", selected_tab(), "\n")
    }, ignoreInit = TRUE)

    # Load data only when hb1 tab is active
    hb1_data <- eventReactive(enabled(), {
      req(enabled())
      load_data("hb1")
    }, ignoreInit = TRUE)

    hb1_plot <- reactive({
      req(enabled())
      d <- hb1_data()

      t <- if (checks$sourcenames) "Daily exchange rates and TWI - HB1" else "Daily exchange rates and TWI"

      valid_inputs <- unique(c(input$Currency1_main, input$Currency2_main)) |> setdiff("-")

      generic_plotly(
        data = d,
        titles = c(t, paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(
          guide_rbnz,
          apply_filters = list(Graph = "hb1", Split = valid_inputs)
        ),
        k = "Date",
        years = 15
      )
    })

    output$plot <- renderPlotly(hb1_plot())
  })
}



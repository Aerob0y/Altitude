mod_hb1_ui_update <- function(id) {
  add_log("mod_hb1_ui_update", "ui function called")

  ns <- NS(id)

  currencies <- filter_series(
    guide,
    column = "Category_1",
    apply_filters = list(Dataset = "hb1")
  ) 
  insert_inputs <- tagList(
    selectInput(
      ns("Currency1_main"),
      "Currency",
      choices = currencies,
      selected = "NZD/USD"
    ),
    selectInput(
      ns("Currency2_main"),
      "Secondary",
      choices = c("-", currencies),
      selected = "-"
    ),
    tags$div(
      class = "dl-compact dl-row",
      download_settings_ui(ns)
    )
  )

  ui_single(
    insert_inputs,
    p = ns("plot"),
    h = "600px",
    module = "hb1"
  )
}

mod_hb1_server_update <- function(id, selected_tab, activate_on) {
  add_log("mod_hb1_server_update", "starting server function")

  moduleServer(id, function(input, output, session) {

    enabled <- reactive(
      identical(selected_tab(), activate_on)
    )

    currencies <- filter_series(
      guide,
      column = "Category_1",
      apply_filters = list(Dataset = "hb1")
    ) 

    observeEvent(input$Currency1_main, {

      updateSelectInput(
        session,
        "Currency2_main",
        choices = c(
          "-",
          setdiff(currencies, input$Currency1_main)
        ),
        selected = if (
          identical(input$Currency2_main, input$Currency1_main)
        ) {
          "-"
        } else {
          input$Currency2_main
        }
      )
    })

    output$plot <- renderPlotly({
      req(enabled())
      req(input$Currency1_main)
      req(input$Currency2_main)

      valid_inputs <- unique(
        c(input$Currency1_main, input$Currency2_main)
      ) |>
        setdiff("-")

      standard_plot(
        data = load_data("hb1"),
        titles = c(
          "Daily exchange rates:",
          paste("RBNZ:", short_title(valid_inputs))
        ),
        series = filter_series(
          guide,
          apply_filters = list(
            Dataset = "hb1",
            Category_1 = valid_inputs
          )
        ),
        split_by = NULL,
        split_columns = NULL,
        k = "Date"
      )
    })
  })
}

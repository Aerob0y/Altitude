# Comparison module for the HM3 data. The hb3_test name is intentionally kept
# separate from hm3 so the existing x_plotly() module remains unchanged.
mod_hb3_test_ui <- function(id) {
  ns <- NS(id)
  choices <- filter_series(
    guide_rbnz,
    column = "Class_1",
    apply_filters = list(Data = "hm3")
  )$Class_1
  choices <- choices[!is.na(choices)]

  ui_single(
    tagList(
      selectInput(
        ns("class_1"),
        "Investment Type (vs)",
        choices = choices,
        selected = choices[1],
        multiple = FALSE
      ),
      selectInput(
        ns("class_1_2"),
        NULL,
        choices = c("-", choices),
        selected = if (length(choices) >= 4) choices[4] else "-",
        multiple = FALSE
      )
    ),
    p = ns("plot"),
    h = "600px",
    module = "hb3_test"
  )
}

mod_hb3_test_server <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hm3_data <- eventReactive(enabled(), {
      req(enabled())
      load_data("hm3")
    }, ignoreInit = FALSE)

    observeEvent(input$class_1, {
      req(input$class_1)
      if (!is.null(input$class_1_2) && input$class_1_2 == input$class_1) {
        updateSelectInput(session, "class_1_2", selected = "-")
      }
    }, ignoreInit = TRUE)

    output$plot <- renderPlotly({
      selected <- unique(c(input$class_1, input$class_1_2))
      selected <- setdiff(selected[!is.na(selected)], "-")
      req(length(selected))

      series <- filter_series(
        guide_rbnz,
        apply_filters = list(Data = "hm3", Class_1 = selected)
      )
      reference <- jsonlite::fromJSON(
        "app/config/hb3_test.json",
        simplifyVector = FALSE
      )
      trace_ids <- vapply(reference$traces, `[[`, character(1), "y")
      reference$traces <- reference$traces[trace_ids %in% series$ID]
      reference$subtitle <- paste("RBNZ:", short_title(selected))

      reference_plot(hm3_data(), reference)
    })
  })
}

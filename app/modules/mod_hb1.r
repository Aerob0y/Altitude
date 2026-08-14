

mod_hb1_ui <- function(id) {
  # ID Setup
  if (checks$ui_module) {print("hb1_ui loaded")}
  ns <- NS(id)
  # Get Metrics
  class_1 <- filter_series(
    guide_rbnz,
    column = "Class_1",
    apply_filters = list(Data = c("hb1"))
  ) %>% as.vector() %>% unlist() %>% unname()

  # Get Inputs
  insert_inputs <- tagList(
    selectInput(ns("Currency1_main"), "Currency", choices = class_1, selected = "NZD/USD"),
    selectInput(ns("Currency2_main"), "Secondary", choices = c("-", class_1), selected = "-"),
    tags$div(
      class = "dl-compact dl-row",
      #mod_download_ui(ns("dl"))
      tags$div(
        class = "dl-wrap",
        actionLink(ns("dl_toggle"), label = "Download Settings", icon = icon("camera"), class = "dl-gear"),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] %% 2 == 1", ns("dl_toggle")),
          tags$div(class = "dl-panel", mod_download_ui(ns("dl")))
        )
      )
    )
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_hb1_server <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))


    # Load data only when hb1 tab is active
    hb1_data <- eventReactive(enabled(), {
      req(enabled())
      print("hb1_server loaded")
      load_data("hb1")
    }, ignoreInit = FALSE)

    #dl2 <- reactive({
    #  return(list(format=input$dl_format, width=input$dl_width, height=input$dl_height,  scale=2))
    #})
    dl <- mod_download_server("dl")


    hb1_plot <- reactive({
      req(enabled())
      d <- hb1_data()

      t <- if (checks$sourcenames) "Daily exchange rates and TWI - HB1" else "Daily exchange rates and TWI"

      valid_inputs <- unique(c(input$Currency1_main, input$Currency2_main)) |> setdiff("-")

      x_plotly(
        data = d,
        titles = c(t, paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(
          guide_rbnz,
          apply_filters = list(Data = "hb1", Class_1 = valid_inputs)
        ),
        k = "Date",
        years = 15,
        download   = dl$download(),
        clean_ui   = dl$clean_export()
      )
    })

    output$plot <- renderPlotly(hb1_plot())
  })
}

# ==============================================================================
# README ui.r -----------------------------------------------------------------------
# set NS
# get metrics
# get any other data
# create ui with inputs and plot
# ==============================================================================
mod_hb1_ui <- function(id) {
  ns <- NS(id)
  class_1 <- filter_series_unlist(guide_rbnz, column = "Class_1", apply_filters = list(Data = c("hb1")))
  ui_single(
    tagList(
      selectInput(ns("Currency1_main"), "Currency", choices = class_1, selected = "NZD/USD"),
      selectInput(ns("Currency2_main"), "Secondary", choices = c("-", class_1), selected = "-"),
      tags$div(class = "dl-compact dl-row", download_settings_ui(ns))
    ),
    p = ns("plot"),
    h = "600px"
  )
}

# ==============================================================================
# README mod__server -----------------------------------------------------------------------
# moduleServer takes three arguments: id, selected_tab, activate_on
#   id: is used to namespace the inputs and outputs of the module, so that they don't conflict with other modules or the main app.
#   selected_tab: a reactive expression telling you which tab is currently selected
#   activate_on: the tab name/value that should activate this module
#   enabled: will return true if the module is active, false otherwise. Use this to control when to load data and render plots.
#   req(enabled()) => Blocks all code after until the module is active.
#
# data: req // load data
# dl: download settings module
# plot: req // create plot
#  valid_inputs: get the selected inputs, remove duplicates and remove any "-" values
#  x_plotly: create the plotly plot, passing in the data, titles, series, k, years, download settings and clean_ui settings
# output$plot: render the plotly plot

# ==============================================================================
mod_hb1_server <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    hb1_data <- eventReactive(enabled(), {
      req(enabled())
      load_data("hb1")
    }, ignoreInit = FALSE)

    dl <- mod_download_server("dl")

    hb1_plot <- reactive({
      req(enabled())
      valid_inputs <- unique(c(input$Currency1_main, input$Currency2_main)) |> setdiff("-")
      x_plotly(
        data = hb1_data(),
        titles = c("Daily exchange rates and TWI", paste("RBNZ:", short_title(valid_inputs))),
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

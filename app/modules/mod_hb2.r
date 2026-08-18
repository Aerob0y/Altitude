# ==============================================================================
# README ui.r -----------------------------------------------------------------------
# set NS
# get metrics
# get any other data
# create ui with inputs and plot
# ==============================================================================
mod_hb2_ui <- function(id) {
  ns <- NS(id)
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
    ),
    tags$div(class = "dl-compact dl-row", download_settings_ui(ns))
  )

  ui_single(insert_inputs, p = ns("plot"), h = "600px")
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

mod_hb2_server <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))


    # Load data only when hb2 tab is active
    hb2_data <- eventReactive(enabled(), {
      req(enabled())
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

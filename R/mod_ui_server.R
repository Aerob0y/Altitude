app_ui <- function() {
  shiny::fluidPage(
    shiny::tags$head(
      shiny::includeCSS("inst/app/www/css/app.css")
    ),
    mod_dashboard_ui("dash")
  )
}

app_server <- function(input, output, session, rbnz) {
  mod_dashboard_server("dash", rbnz = rbnz)
}

mod_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(shiny::selectInput(ns("series"), "Series", choices = names(rbnz_choices()))),
    shiny::mainPanel(plotly::plotlyOutput(ns("p")))
  )
}

mod_dashboard_server <- function(id, rbnz) {
  shiny::moduleServer(id, function(input, output, session) {
    output$p <- plotly::renderPlotly({
      utils_plot_rbnz(rbnz, input$series) # lives in R/utils_plot.R
    })
  })
}

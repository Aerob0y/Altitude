# Initialization script for the Shiny application
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(bslib)))
suppressMessages(suppressWarnings(library(plotly)))
suppressMessages(suppressWarnings(library(thematic)))



run_app <- function() {
  source("R/utils_data.r", local = FALSE)
  source("R/shiny/ui.r", local = FALSE)
  source("R/shiny/server.r", local = FALSE)
  source("R/plotly_elements.r", local = FALSE)
  #rbnz <- readRDS("data/rbnz.rds")
  #server <- function(input, output, session) app_server(input, output, session, rbnz)
  shiny::shinyApp(ui, server, options = list(port = 5555))
}

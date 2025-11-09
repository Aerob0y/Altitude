# Initialization script for the Shiny application
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(bslib)))
suppressMessages(suppressWarnings(library(plotly)))
suppressMessages(suppressWarnings(library(thematic)))

run_app <- function() {
  # load packaged data / config up front
  #rbnz <- readRDS("data/rbnz.rds")
  ui <- app_ui()
  server <- function(input, output, session) app_server(input, output, session, rbnz)
  shiny::shinyApp(ui, server)
}

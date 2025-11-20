# Initialization script for the Shiny application
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(bslib)))
suppressMessages(suppressWarnings(library(plotly)))
suppressMessages(suppressWarnings(library(thematic)))
suppressMessages(suppressWarnings(library(memoise)))


run_app <- function() {
  source("r/utils_data.r")
  source("r/plotly_elements.r")
  source("r/shiny/server.r")
  source("r/shiny/ui.r")

  shiny::shinyApp(ui, server, options = list(port = 5555))
}

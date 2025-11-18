# Initialization script for the Shiny application
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(bslib)))
suppressMessages(suppressWarnings(library(plotly)))
suppressMessages(suppressWarnings(library(thematic)))



run_app <- function() {
  source("r/utils_data.r", local = FALSE)
  source("r/plotly_elements.r", local = FALSE)
  source("r/plotly_rbnz.r", local = FALSE)
  source("r/shiny/ui.r", local = FALSE)
  source("r/shiny/server.r", local = FALSE)
  shiny::shinyApp(ui, server, options = list(port = 5555))
}

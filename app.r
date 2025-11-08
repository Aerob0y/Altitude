
# Initialization script for the Shiny application
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(bslib)))
suppressMessages(suppressWarnings(library(plotly)))
suppressMessages(suppressWarnings(library(thematic)))

#load style components


source("styles/plotly_elements.r")
source("helper/helper.r")

# Load source files for UI and server components
source("shiny/ui.r")  # Assumes hb2_plot() returns a Plotly object
source("shiny/server.r")  # Assumes hb2_plot() returns a Plotly object

# Run the Shiny application
shinyApp(ui = ui, server = server, options = list(port = 5555))

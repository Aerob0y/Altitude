# Initialization script for the Shiny application


cache <- new.env(parent = emptyenv())
guide_rbnz <- readRDS("reference/RBNZ_Series.rds")

run_app <- function() {
  # Libraries
  # datasets

  source("r/utils_data.r")
  source("r/plotly_elements.r")

  # ui elements
  source("r/shiny/ui/ui_elements.r")
  source("r/shiny/ui/ui_rbnz.r")
  source("r/shiny/ui/ui_slides.r")

  module_files <- list.files("R/shiny/modules", full.names = TRUE, pattern = "\\.R$")
  lapply(module_files, source)

  #server elements
  source("r/shiny/server/server_rbnz.r")
  #shiny
  source("r/shiny/server.r")
  source("r/shiny/ui.r")

  shiny::shinyApp(ui, server, options = list(port = 5555))
}

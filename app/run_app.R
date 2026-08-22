# Initialization script for the Shiny application
source("app/utils/load_utils.r") # shared dependencies and helpers
source("app/ui/ui_elements.r") # common UI elements
source("app/ui/ui_slides.r")
# Load modules
purrr::walk(
  list.files("app/modules", full.names = TRUE, pattern = "\\.r$"),
  ~ source(.x, local = FALSE, echo = FALSE)
)

# Load UI and Server
source("app/server.r")
source("app/ui.r")

# Function to run the app
run_app <- function() {shiny::shinyApp(ui, server, options = list(port = 5555))}
run_app()


guide_rbnz <- load_cached_rds(
  "app/data/Reference/RBNZ_Series.csv",
  "app/data/Reference/RBNZ_Series.rds"
)

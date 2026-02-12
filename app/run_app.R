# Initialization script for the Shiny application
#`%||%` <- function(x, y) if (is.null(x)) y else x


source("app/utils/utils_data.r") # data sources, caching, filtering
source("app/utils/plotly_elements.r") # plotly customisations
source("app/ui/ui_elements.r") # common UI elements

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
# Initialization script for the Shiny application
source("app/utils/core/load_utils.r") # shared dependencies and helpers
source("app/ui/ui_elements.r") # common UI elements
source("app/ui/ui_slides.r")

# Load UI and Server
source("app/server.r")
source("app/ui.r")



# Function to run the app
run_app <- function() {shiny::shinyApp(ui, server, options = list(port = 5555))}
run_app()

exists("mod_hm1_server_update")

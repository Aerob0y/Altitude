# Initialization script for the Shiny application
checks <- list(
  load_data = FALSE,
  filter_series = FALSE,
  ui_elements = FALSE,
  sourcenames = TRUE,
  ui_module = TRUE
)
source("r/utils/open_lib.r") # Load required libraries
source("r/utils/utils_data.r") # data sources, caching, filtering
source("r/utils/plotly_elements.r") # plotly customisations
source("r/shiny/ui/ui_elements.r") # common UI elements

list.files("r/shiny/modules", full.names = TRUE, pattern = "\\.r$") %>% lapply(source)  # load all modules

source("r/shiny/ui/ui_slides.r") # slides UI elements

source("r/shiny/server.r")
source("r/shiny/ui.r")

run_app <- function() {
  shiny::shinyApp(ui, server, options = list(port = 5555))
}
run_app()



#source("r/shiny/ui/ui_rbnz.r")
#source("r/shiny/server/server_rbnz.r")
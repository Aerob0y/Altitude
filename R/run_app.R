# Initialization script for the Shiny application


#suppressMessages(suppressWarnings(library(ggplot2)))
#suppressMessages(suppressWarnings(library(bslib)))
#suppressMessages(suppressWarnings(library(plotly)))
#suppressMessages(suppressWarnings(library(thematic)))


run_app <- function() {
  #suppressMessages(suppressWarnings(library(dplyr)))
  suppressMessages(suppressWarnings(library(memoise)))
  suppressMessages(suppressWarnings(library(shiny)))
  suppressMessages(suppressWarnings(library(bslib, include.only = c("navset_tab", "nav_menu", "nav_panel", "page_sidebar","sidebar","card"))))
  suppressMessages(suppressWarnings(library(stats, include.only = c("setNames", "na.omit", "complete.cases"))))
  suppressMessages(suppressWarnings(library(dplyr, include.only = c("filter", "group_by", "summarise", "collapse", "all_of", "collapse",  "select"))))
  suppressMessages(suppressWarnings(library(tibble, include.only = c("as_tibble", "deframe"))))
  suppressMessages(suppressWarnings(library(stringr, include.only = c("str_replace_all"))))
  suppressMessages(suppressWarnings(library(plotly, include.only = c("renderPlotly","plotlyOutput","config","layout","add_trace","subplot"))))
  source("r/utils_data.r")
  source("r/plotly_elements.r")
  source("r/shiny/server.r")
  source("r/shiny/ui.r")

  shiny::shinyApp(ui, server, options = list(port = 5555))
}
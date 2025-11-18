
source("r/plotly_fuel.r")
ui_fuel <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      style = "height: 100%;"
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('fuel_plot',  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
  ), style = "background-color: #EDF2F3 !important; height: 600px;"
)

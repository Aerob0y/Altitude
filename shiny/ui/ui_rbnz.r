

source("plotly/plotly_rbnz.r")


.hb1_split <- rbnz_series(col = "Split", filter_graph = "hb1")
ui_hb1x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("Currency1", "Currency",  choices = Filter(Negate(is.na), .hb1_split), selected = 'NZD/USD', multiple = FALSE),
      selectInput("Currency2", "Second Currency", choices = c("-", Filter(Negate(is.na), .hb1_split)), selected = '-', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hb1_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

.hb2_split <- rbnz_series(col = "Names", filter_graph = "hb2")
ui_hb2x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      checkboxGroupInput("hb2_input", "Rates",  choices = Filter(Negate(is.na), .hb2_split), selected = Filter(Negate(is.na), .hb2_split))
      #selectInput("y", "yy:", choices = c("A","B"), selected = 'A', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hb2_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

ui_hc35x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("x", "xx",  choices = c("A","B"), selected = 'A', multiple = FALSE),
      selectInput("y", "yy:", choices = c("A","B"), selected = 'A', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hc35_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

.hm1_split <- rbnz_series(col = "Names", filter_graph = "hm1")
.hm1_metrics <- rbnz_series(col = "Dim", filter_graph = "hm1")
ui_hm1x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      checkboxGroupInput("hm1_input", "Series",  choices = Filter(Negate(is.na), .hm1_split), selected = 'Consumers price index (CPI)'),
      selectInput("hm1_metric", "Metric:", choices = Filter(Negate(is.na), .hm1_metrics), selected = '(Index)')
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm1_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

.hm2_split <- rbnz_series(col = 'Split', filter_graph = 'hm2')
.hm2_dim <- rbnz_series(col = 'Dim', filter_graph = 'hm2')

ui_hm2x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      checkboxGroupInput("hm2_split", "xx",  choices = Filter(Negate(is.na), .hm2_split), selected = c('Retail trade sales')),
      selectInput("hm2_dim", "yy:", choices = Filter(Negate(is.na), .hm2_dim), selected = '% s.a.', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm2_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)


.hm3_split <- rbnz_series(col = 'Split', filter_graph = 'hm3')
.hm3_dim <- rbnz_series(col = 'Dim', filter_graph = 'hm3')
ui_hm3x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hm3_split", "xx",  choices = Filter(Negate(is.na), .hm3_split), selected = 'Building consents - residential dwellings'),
      selectInput("hm3_dim", "yy:", choices = Filter(Negate(is.na), .hm3_dim), selected = 'NZD/USDx')
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm3_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

.hm4_split <- rbnz_series(col = 'Split', filter_graph = 'hm4')
.hm4_dim <- rbnz_series(col = 'Dim', filter_graph = 'hm4')
.hm4_group <- rbnz_series(col = 'Grouping', filter_graph = 'hm4')
ui_hm4x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hm4_dim", "xx",  choices =  Filter(Negate(is.na), .hm4_dim), selected = '($m s.a.)'),
      selectInput("hm4_group", "xx",  choices =  Filter(Negate(is.na), .hm4_group), selected = 'Operating income'),
      checkboxGroupInput("hm4_split", "yy:", choices = Filter(Negate(is.na), .hm4_split), selected = 'Manufacturing operating income')

    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm4_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

.hm5_split <- rbnz_series(col = 'Split', filter_graph = 'hm5')
.hm5_dim <- rbnz_series(col = 'Dim', filter_graph = 'hm5')
ui_hm5x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hm5_split", "xx",  choices =  Filter(Negate(is.na), .hm5_split), selected = 'Production-based gross domestic product (GDP)'),
      selectInput("hm5_dim", "yy:", choices =  Filter(Negate(is.na), .hm5_dim), selected = '(Real $m)')
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm5_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

ui_hm6x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("x", "xx",  choices = .hb1_split, selected = 'NZD/AUDx', multiple = FALSE),
      selectInput("y", "yy:", choices = .hb1_split, selected = 'NZD/USDx', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm6_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

ui_hm7x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("x", "xx",  choices = .hb1_split, selected = 'NZD/AUDx', multiple = FALSE),
      selectInput("y", "yy:", choices = .hb1_split, selected = 'NZD/USDx', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm7_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

ui_hm8x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("xx", "xx",  choices = .hb1_split, selected = 'NZD/AUDx', multiple = FALSE),
      selectInput("yy", "yy:", choices = .hb1_split, selected = 'NZD/USDx', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm8_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

ui_hm9x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("x", "xx",  choices = .hb1_split, selected = 'NZD/AUDx', multiple = FALSE),
      selectInput("y", "yy:", choices = .hb1_split, selected = 'NZD/USDx', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm9_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

ui_hm10x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("x", "xx",  choices = .hb1_split, selected = 'NZD/AUDx', multiple = FALSE),
      selectInput("y", "yy:", choices = .hb1_split, selected = 'NZD/USDx', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('h10_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

ui_hm14x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("x", "xx",  choices = .hb1_split, selected = 'NZD/AUDx', multiple = FALSE),
      selectInput("y", "yy:", choices = .hb1_split, selected = 'NZD/USDx', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm14_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

ui_hs32x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("x", "xx",  choices = .hb1_split, selected = 'NZD/AUDx', multiple = FALSE),
      selectInput("y", "yy:", choices = .hb1_split, selected = 'NZD/USDx', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hs32_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)
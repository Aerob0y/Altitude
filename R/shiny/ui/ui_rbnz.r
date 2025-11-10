

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

.hb2_split <- rbnz_series(col = "Split", filter_graph = "hb2")
.hb2_group <- rbnz_series(col = "Grouping", filter_graph = "hb2")

ui_hb2x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      checkboxGroupInput("hb2_split", "Rates",  choices = Filter(Negate(is.na), .hb2_split), selected = Filter(Negate(is.na), .hb2_split)[1:5]),
      selectInput("hb2_group", "Grouping", choices = Filter(Negate(is.na), .hb2_group), selected = Filter(Negate(is.na), .hb2_group)[1], multiple = FALSE)
      #selectInput("y", "yy:", choices = c("A","B"), selected = 'A', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hb2_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

.hc35_group <- rbnz_series(col = 'Grouping', filter_graph = 'hc35')
.hc35_split <- rbnz_series(col = 'Split', filter_graph = 'hc35')
.hc35_adj <- rbnz_series(col = 'Adj', filter_graph = 'hc35')

ui_hc35x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hc35_adj", "Adj",  choices = Filter(Negate(is.na), .hc35_adj), selected = Filter(Negate(is.na), .hc35_adj)[1], multiple = FALSE),
      selectInput("hc35_group", "Grouping",  choices = Filter(Negate(is.na), .hc35_group), selected = Filter(Negate(is.na), .hc35_group), multiple = TRUE),
      selectInput("hc35_split", "Split", choices = Filter(Negate(is.na), .hc35_split), selected = Filter(Negate(is.na), .hc35_split), multiple = TRUE)
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
      selectInput("hm1_metric", "Index:", choices = Filter(Negate(is.na), .hm1_metrics), selected = 'y/y%'),
      checkboxGroupInput("hm1_input", "Price Measure:",  choices = Filter(Negate(is.na), .hm1_split), selected = Filter(Negate(is.na), .hm1_split)[1:5])
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
      #checkboxGroupInput("hm2_split", "xx",  choices = Filter(Negate(is.na), .hm2_split), selected = Filter(Negate(is.na), .hm2_split)[1]),
      selectInput("hm2_split", "Series 1:", choices = Filter(Negate(is.na), .hm2_split), selected = 'Retail trade sales NZDm(r) s.a.', multiple = FALSE),
      selectInput("hm2_split2", "Series 2:", choices = c("-", Filter(Negate(is.na), .hm2_split)), selected = 'Retail trade volumes NZDm(r) s.a.', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm2_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

# Investment
.hm3_split <- rbnz_series(col = 'Split', filter_graph = 'hm3')
.hm3_dim <- rbnz_series(col = 'Dim', filter_graph = 'hm3')
ui_hm3x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hm3_split", "Series 1:",  choices = Filter(Negate(is.na), .hm3_split), selected = .hm3_split[1]),
      selectInput("hm3_split2", "Series 2:", choices = c("-", Filter(Negate(is.na), .hm3_split)), selected = "-")
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


# Get the list of groups (splits)
splits <- stats::na.omit(rbnz_series(col = "Split", filter_graph = "hm5"))

# For each split, collect its adjs and map to labels, producing a named vector:
# names = labels shown in the UI, values = the value returned by input$state
choices1 <- setNames(
  lapply(splits, function(split) {
    adjs <- stats::na.omit(rbnz_series(col = "Adj", filter_graph = "hm5", split = split))

    labels <- vapply(adjs, function(adj) {
      nm <- rbnz_series(col = "Names", filter_graph = "hm5", adj = adj)#split = split
      nm <- stats::na.omit(nm)
      if (length(nm)) nm[1] else adj
    }, character(1))

    # IMPORTANT: names are labels, values are returned values
    stats::setNames(adjs, labels)
  }),
  splits
)
choices1$`Change in inventories`


.hm5_split <- rbnz_series(col = 'Split', filter_graph = 'hm5')
.hm5_dim <- rbnz_series(col = 'Adj', filter_graph = 'hm5')
.hm5_names <- rbnz_series(col = 'Names', filter_graph = 'hm5')
ui_hm5x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      #selectInput("hm5_split1", "Metric1",  choices =  c("-", Filter(Negate(is.na), .hm5_split)), selected = 'Imports of goods and services'),
      #selectInput("hm5_dim1", "Show As:", choices =  c("-",Filter(Negate(is.na), .hm5_dim)), selected = 'Real $m'),
      #selectInput("hm5_split2", "Metric2",  choices =  c("-", Filter(Negate(is.na), .hm5_split)), selected = 'Exports of goods and services'),
      #selectInput("hm5_dim2", "Show As:", choices =  c("-",Filter(Negate(is.na), .hm5_dim)), selected = 'Real $m'),
      #selectInput("hm5_names", "All options", choices =  Filter(Negate(is.na), .hm5_names), selected = "", multiple = TRUE),
      selectInput("hm5_names", "xx",  choices = names(choices1), multiple = TRUE, selected = names(choices1[[1]])[1])
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm5_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)


.hm6_names <- rbnz_series(col = 'Names', filter_graph = 'hm6')
ui_hm6x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hm6_names", "Series",  choices = Filter(Negate(is.na), .hm6_names), selected = Filter(Negate(is.na), .hm6_names)[1], multiple = TRUE)
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

.hm8_adj <- rbnz_series(col = 'Adj', filter_graph = 'hm8')
.hm8_split <- rbnz_series(col = 'Split', filter_graph = 'hm8')

ui_hm8x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hm8_split", "Split",  choices = Filter(Negate(is.na), .hm8_split), selected = Filter(Negate(is.na), .hm8_split)[1], multiple = TRUE),
      selectInput("hm8_adj", "Dim", choices = Filter(Negate(is.na), .hm8_adj), selected = Filter(Negate(is.na), .hm8_adj)[1], multiple = TRUE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm8_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

.hm9_adj <- rbnz_series(col = 'Adj', filter_graph = 'hm9')
.hm9_split <- rbnz_series(col = 'Split', filter_graph = 'hm9')
ui_hm9x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hm9_split", "Split",  choices = Filter(Negate(is.na), .hm9_split), selected = Filter(Negate(is.na), .hm9_split)[1], multiple = TRUE),
      selectInput("hm9_adj", "Dim", choices = Filter(Negate(is.na), .hm9_adj), selected = Filter(Negate(is.na), .hm9_adj)[1], multiple = TRUE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm9_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

.hm10_split <- rbnz_series(col = 'Split', filter_graph = 'hm10')
ui_hm10x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hm10_split_1", "Metric 1",  choices = Filter(Negate(is.na), .hm10_split), selected = Filter(Negate(is.na), .hm10_split)[1], multiple = FALSE),
      selectInput("hm10_split_2", "Metric 2",  choices = Filter(Negate(is.na), .hm10_split), selected = Filter(Negate(is.na), .hm10_split)[2], multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm10_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

.hm14_split <- rbnz_series(col = 'Split', filter_graph = 'hm14')
.hm14_group <- rbnz_series(col = 'Grouping', filter_graph = 'hm14')
ui_hm14x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hm14_split", "Split",  choices = Filter(Negate(is.na), .hm14_split), selected = Filter(Negate(is.na), .hm14_split)[1], multiple = TRUE),
      selectInput("hm14_group", "Group",  choices = Filter(Negate(is.na), .hm14_group), selected = Filter(Negate(is.na), .hm14_group)[1], multiple = TRUE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm14_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)


.hs32_group <- rbnz_series(col = 'Grouping', filter_graph = 'hs32')
.hs32_split <- rbnz_series(col = 'Split', filter_graph = 'hs32')
.hs32_adj <- rbnz_series(col = 'Adj', filter_graph = 'hs32')
ui_hs32x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hs32_adj", "Breakdown",  choices = Filter(Negate(is.na), .hs32_adj), selected = Filter(Negate(is.na), .hs32_adj)[2], multiple = FALSE),
      selectInput("hs32_group", "Grouping",  choices = Filter(Negate(is.na), .hs32_group), selected = Filter(Negate(is.na), .hs32_group)[1], multiple = TRUE),
      selectInput("hs32_split", "Split:", choices = Filter(Negate(is.na), .hs32_split), selected = Filter(Negate(is.na), .hs32_split), multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hs32_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)
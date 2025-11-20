

source("r/plotly_rbnz.r")


.hb1_split <- rbnz_series(col = "Split", filter_graph = "hb1")
ui_hb1x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("Currency1", "Currency",  choices = Filter(Negate(is.na), .hb1_split), selected = 'NZD/USD', multiple = FALSE),
      selectInput("Currency2", "Second Currency", choices = c("-", Filter(Negate(is.na), .hb1_split)), selected = '-', multiple = FALSE),
      style = "height: 100%;"
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hb1_plotx',  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
  ), style = "background-color: #EDF2F3 !important; height: 600px;"
)


.hb2_split <- Filter(Negate(is.na), rbnz_series(col = "Split", filter_graph = "hb2"))
.hb2_group <- Filter(Negate(is.na), rbnz_series(col = "Grouping", filter_graph = "hb2"))
.hb2_tier <- list()
for (i in seq_along(.hb2_group)) {
  .hb2_tier[[.hb2_group[i]]] <- Filter(Negate(is.na), rbnz_series(col = "Names", filter_graph = "hb2", filter_group = .hb2_group[i]))
}
.hb2_tier

ui_hb2x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      #checkboxGroupInput("hb2_tier", "Select", choices = .hb2_tier, selected =  .hb2_tier$'Swap rates close')
      selectInput("hb2_tier", "Select", choices = .hb2_tier, selected =  c("Official Cash Rate (OCR)", .hb2_tier$'Swap rates close'[c(1,2,3,4)]), multiple = TRUE)
      #checkboxGroupInput("hb2_split", "Rates",  choices = Filter(Negate(is.na), .hb2_split), selected = Filter(Negate(is.na), .hb2_split)[1:5]),
      #selectInput("hb2_group", "Grouping", choices = Filter(Negate(is.na), .hb2_group), selected = Filter(Negate(is.na), .hb2_group)[1], multiple = FALSE)
      #selectInput("y", "yy:", choices = c("A","B"), selected = 'A', multiple = FALSE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hb2_plotx',  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%; padding: 0px; margin: 0px;")
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)




.hc35_group <- rbnz_series(col = 'Grouping', filter_graph = 'hc35')
.hc35_split <- rbnz_series(col = 'Split', filter_graph = 'hc35')
.hc35_adj <- rbnz_series(col = 'Adj', filter_graph = 'hc35')

ui_hc35x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      #radioButtons("hc35_adj", "Adj",  choices = Filter(Negate(is.na), .hc35_adj), selected = Filter(Negate(is.na), .hc35_adj)[1]),
      radioButtons("hc35_group", "Grouping",  choices = Filter(Negate(is.na), .hc35_group), selected = Filter(Negate(is.na), .hc35_group)[1]),
      checkboxGroupInput("hc35_split", "Split:", choices = Filter(Negate(is.na), .hc35_split), selected = Filter(Negate(is.na), .hc35_split)[c(1,3,4)] )
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

#.hm4_split <- rbnz_series(col = 'Split', filter_graph = 'hm4')
#.hm4_dim <- rbnz_series(col = 'Dim', filter_graph = 'hm4')
.hm4_group <- rbnz_series(col = 'Grouping', filter_graph = 'hm4')
ui_hm4x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      #selectInput("hm4_dim", "xx",  choices =  Filter(Negate(is.na), .hm4_dim), selected = '($m s.a.)'),
      selectInput("hm4_group", "Series:",  choices =  Filter(Negate(is.na), .hm4_group), selected = 'Operating income')
      #checkboxGroupInput("hm4_split", "yy:", choices = Filter(Negate(is.na), .hm4_split), selected = 'Manufacturing operating income')

    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm4_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

.hm5_split <- rbnz_series(col = "Split", filter_graph = "hm5")
.hm5_split <- .hm5_split[!is.na(.hm5_split)]  # drop NAs

.hm5_choices <- lapply(.hm5_split, function(split) {
  # One split at a time
  names_vec <- rbnz_series(
    col          = "Names",
    filter_graph = "hm5",
    split        = split
  )

  # Codes like A1, A2, ..., using u = FALSE as you did
  adj_codes <- rbnz_series(
    col          = "Adj",
    filter_graph = "hm5",
    split        = split,
    u            = FALSE
  )

  # This creates a *named vector*: names = labels, values = codes
  setNames(adj_codes, names_vec)
})

names(.hm5_choices) <- .hm5_split

ui_hm5x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      #selectInput("hm5_split1", "Metric1",  choices =  c("-", Filter(Negate(is.na), .hm5_split)), selected = 'Imports of goods and services'),
      #selectInput("hm5_dim1", "Show As:", choices =  c("-",Filter(Negate(is.na), .hm5_dim)), selected = 'Real $m'),
      #selectInput("hm5_split2", "Metric2",  choices =  c("-", Filter(Negate(is.na), .hm5_split)), selected = 'Exports of goods and services'),
      #selectInput("hm5_dim2", "Show As:", choices =  c("-",Filter(Negate(is.na), .hm5_dim)), selected = 'Real $m'),
      #selectInput("hm5_names", "All options", choices =  Filter(Negate(is.na), .hm5_names), selected = "", multiple = TRUE),
      selectInput("hm5_names", "Series:",  choices = .hm5_choices, multiple = TRUE, .hm5_choices)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm5_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)


.hm6_names <- rbnz_series(col = 'Names', filter_graph = 'hm6')
ui_hm6x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hm6_names", "Group",  choices = Filter(Negate(is.na), .hm6_names), selected = Filter(Negate(is.na), .hm6_names)[1], multiple = TRUE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm6_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)

.hm7_group <- rbnz_series(col = 'Grouping', filter_graph = 'hm7')
ui_hm7x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hm7_group", "Series",  choices = Filter(Negate(is.na), .hm7_group), selected = Filter(Negate(is.na), .hm7_group)[1], multiple = TRUE)
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm7_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)


.hm8_group <- rbnz_series(col = 'Grouping', filter_graph = 'hm8')
.hm8_split <- rbnz_series(col = 'Split', filter_graph = 'hm8')

ui_hm8x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      selectInput("hm8_group1", "Dim",  choices = Filter(Negate(is.na), .hm8_group), selected = Filter(Negate(is.na), .hm8_group)[1], multiple = FALSE),
      selectInput("hm8_group2", "Dim", choices = c("-", Filter(Negate(is.na), .hm8_group)), selected = "-", multiple = FALSE),
      selectInput("hm8_split", "Split", choices = c("-", Filter(Negate(is.na), .hm8_split)), selected = "-", multiple = FALSE)
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
      radioButtons("hm14_split", "Split",  choices = Filter(Negate(is.na), .hm14_split), selected = Filter(Negate(is.na), .hm14_split)[1]),
      checkboxGroupInput("hm14_group", "Group",  choices = Filter(Negate(is.na), .hm14_group), selected = Filter(Negate(is.na), .hm14_group)[1])
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hm14_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)


.hs32_group <- rbnz_series(col = 'Grouping', filter_graph = 'hs32')
.hs32_split <- rbnz_series(col = 'Split', filter_graph = 'hs32')
.hs32_adj <- rbnz_series(col = 'Adj', filter_graph = 'hs32')
x <- c("Total", "Use", "Type")
ui_hs32x <- fluidPage(
  page_sidebar(
    sidebar = sidebar(
      class = 'csv-sidebar', position = "right",
      radioButtons("hs32_adj", "Breakdown",  choices = Filter(Negate(is.na), x), selected = Filter(Negate(is.na), x)[2]),
      checkboxGroupInput("hs32_group", "Grouping",  choices = Filter(Negate(is.na), .hs32_group), selected = Filter(Negate(is.na), .hs32_group)),
      checkboxGroupInput("hs32_split", "Split:", choices = Filter(Negate(is.na), .hs32_split), selected = Filter(Negate(is.na), .hs32_split))
    ),
    card(class = 'csv-card', full_screen = TRUE, plotlyOutput('hs32_plotx',  height = "100%"))
  ), style = "background-color: #EDF2F3 !important; width: 100%;"
)
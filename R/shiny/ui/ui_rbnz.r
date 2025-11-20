# hb1 Exchange rates UI
ui_hb1 <- memoise(function() {
  hb1_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hb1")))
  ui_hb1 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("Currency1", "Currency",  choices = Filter(Negate(is.na), hb1_split), selected = "NZD/USD", multiple = FALSE),
        selectInput("Currency2", "Second Currency", choices = c("-", Filter(Negate(is.na), hb1_split)), selected = "-", multiple = FALSE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hb1_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})
# hb2 Interest rates UI
ui_hb2 <- memoise(function() {
  hb2_tier <- filter_series(guide_rbnz, column = c("Group", "Names"), apply_filters = list(Graph = c("hb2"))) %>%
    group_by(Group) %>%
    summarise(value = list(Names)) %>%
    deframe()

  ui_hb2 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hb2_tier", "Interest Rate Tier",  choices = hb2_tier, selected = hb2_tier, multiple = TRUE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hb2_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})

# hm1 Prices UI
ui_hm1 <- memoise(function() {
  hm1_input <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm1")))
  hm1_metric <- filter_series(guide_rbnz, column = "Dim", apply_filters = list(Graph = c("hm1")))

  ui_hm1 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hm1_input", "Price Type",  choices = hm1_input, selected = hm1_input[1], multiple = FALSE),
        selectInput("hm1_metric", "Metric",  choices = hm1_metric, selected = hm1_metric[1], multiple = FALSE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hm1_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})

# hm2 Consumption UI
ui_hm2 <- memoise(function() {
  hm2_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm2")))
  ui_hm2 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hm2_split", "Consumption Type",  choices = hm2_split, selected = hm2_split[1], multiple = FALSE),
        selectInput("hm2_split2", "Second Consumption Type",  choices = c("-", hm2_split), selected = "-", multiple = FALSE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hm2_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})

# hm3 Investment UI
ui_hm3 <- memoise(function() {
  hm3_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm3")))
  ui_hm3 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hm3_split", "Investment Type",  choices = hm3_split, selected = hm3_split[1], multiple = FALSE),
        selectInput("hm3_split2", "Second Investment Type",  choices = c("-", hm3_split), selected = "-", multiple = FALSE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hm3_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})
# hm4 Domestic Trade UI
ui_hm4 <- memoise(function() {
  hm4_group <- filter_series(guide_rbnz, column = "Group", apply_filters = list(Graph = c("hm4")))
  ui_hm4 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hm4_group", "Trade Group",  choices = hm4_group, selected = hm4_group[1], multiple = FALSE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hm4_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})
# hm5 GDP UI
ui_hm5 <- memoise(function() {
  hm5_names <- filter_series(guide_rbnz, column = "Names", apply_filters = list(Graph = c("hm5")))
  ui_hm5 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hm5_names", "GDP Measures",  choices = hm5_names, selected = c("Export of Goods & Services (Nominal $m)"), multiple = TRUE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hm5_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})
# hm6 National Saving UI
ui_hm6 <- memoise(function() {
  hm6_names <- filter_series(guide_rbnz, column = "Names", apply_filters = list(Graph = c("hm6")))
  ui_hm6 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hm6_names", "Saving Types",  choices = hm6_names, selected = c("National Saving"), multiple = TRUE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hm6_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})
# hm7 Balance of Payments UI
ui_hm7 <- memoise(function() {
  hm7_group <- filter_series(guide_rbnz, column = "Group", apply_filters = list(Graph = c("hm7")))
  ui_hm7 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hm7_group", "Balance Group",  choices = hm7_group, selected = hm7_group[1], multiple = FALSE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hm7_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})

# hm8 Overseas Trade UI
ui_hm8 <- memoise(function() {
  hm8_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm8")))
  hm8_dim <- filter_series(guide_rbnz, column = "Dim", apply_filters = list(Graph = c("hm8")))
  ui_hm8 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hm8_split", "Trade Type",  choices = hm8_split, selected = hm8_split[1], multiple = FALSE),
        selectInput("hm8_dim", "Metric",  choices = hm8_dim, selected = hm8_dim[1], multiple = FALSE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hm8_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})
# hm9 Labour Market UI
ui_hm9 <- memoise(function() {
  hm9_adj <- filter_series(guide_rbnz, column = "Adj", apply_filters = list(Graph = c("hm9")))
  hm9_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm9")))
  ui_hm9 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hm9_adj", "Adjustment Type",  choices = hm9_adj, selected = hm9_adj[1], multiple = FALSE),
        selectInput("hm9_split", "Labour Market Type",  choices = hm9_split, selected = hm9_split[1], multiple = FALSE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hm9_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})
# hm10 Housing UI
ui_hm10 <- memoise(function() {
  hm10_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm10")))
  ui_hm10 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hm10_split_1", "Housing Type",  choices = hm10_split, selected = hm10_split[1], multiple = FALSE),
        selectInput("hm10_split_2", "Second Housing Type",  choices = c("-", hm10_split), selected = "-", multiple = FALSE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hm10_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})
# hm14 Expectations UI
ui_hm14 <- memoise(function() {
  hm14_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm14")))
  hm14_group <- filter_series(guide_rbnz, column = "Group", apply_filters = list(Graph = c("hm14")))
  ui_hm14 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hm14_split", "Expectation Type",  choices = hm14_split, selected = hm14_split[1], multiple = TRUE),
        selectInput("hm14_group", "Time Horizon",  choices = hm14_group, selected = c("1 year out", "2 years out","5 years out"), multiple = TRUE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hm14_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})
# hs32 Loans UI
ui_hs32 <- memoise(function() {
  hs32_adj <- filter_series(guide_rbnz, column = "Adj", apply_filters = list(Graph = c("hs32")))
  hs32_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hs32")))
  hs32_group <- filter_series(guide_rbnz, column = "Group", apply_filters = list(Graph = c("hs32")))
  ui_hs32 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hs32_adj", "Adjustment Type",  choices = hs32_adj, selected = hs32_adj[1], multiple = FALSE),
        selectInput("hs32_split", "Loan Use Type",  choices = hs32_split, selected = hs32_split[1], multiple = TRUE),
        selectInput("hs32_group", "Loan Type",  choices = hs32_group, selected = hs32_group[1], multiple = TRUE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hs32_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})
# hs35 Retail Sales UI
ui_hc35 <- memoise(function() {
  hc35_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hc35")))
  ui_hc35 <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("hc35_split", "Retail Sales Type",  choices = hc35_split, selected = hc35_split[1], multiple = FALSE),
        style = "height: 100%;"
      ),
      card(class = "csv-card", full_screen = TRUE, plotlyOutput("hc35_plot",  height = "100%"), style = "height: 600px; max-width: 900px; width: 100%;")
    ), style = "background-color: #EDF2F3 !important; height: 600px;"
  )
})

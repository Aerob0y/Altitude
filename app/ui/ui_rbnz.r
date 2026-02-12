# First set of controls + plot
ui_hb1_main_remove <- function() {
  hb1_split <- filter_series(guide_rbnz, column = "Split",
                             apply_filters = list(Graph = c("hb1")))
  insert_inputs <- tagList(
    selectInput("Currency1_main",  "Currency",        choices = hb1_split,
                selected = "NZD/USD", multiple = FALSE),
    selectInput("Currency2_main",  "Second Currency", choices = c("-", hb1_split),
                selected = "-",     multiple = FALSE)
  )
  ui_single(insert_inputs, p = "hb1_plot_main", h = "600px")
}

# Second set of controls + plot (e.g. in another tab/box)
ui_hb1_alt_remove <- function() {
  hb1_split <- filter_series(guide_rbnz, column = "Split",
                             apply_filters = list(Graph = c("hb1")))
  insert_inputs <- tagList(
    selectInput("Currency1_alt",  "Currency",        choices = hb1_split,
                selected = "NZD/USD", multiple = FALSE),
    selectInput("Currency2_alt",  "Second Currency", choices = c("-", hb1_split),
                selected = "-",     multiple = FALSE)
  )
  ui_single(insert_inputs, p = "hb1_plot_main2", h = "600px")
}

# hm1 Prices UI
ui_hm1_remove <- memoise(function() {
  hm1_input <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm1")))
  hm1_metric <- filter_series(guide_rbnz, column = "Dim", apply_filters = list(Graph = c("hm1")))
  insert_inputs <- tagList(
    selectInput("hm1_metric", "Metric",  choices = hm1_metric, selected = "y/y%", multiple = FALSE),
    checkboxGroupInput("hm1_input", "Price Index (5 Max)",  choices = hm1_input, selected = hm1_input[c(1, 4, 5, 6)])
  )
  ui_single(insert_inputs, p = "hm1_plot", h = "600px")
})

# hm2 Consumption UI
ui_hm2 <- memoise(function() {
  hm2_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm2")))
  insert_inputs <- tagList(
    selectInput("hm2_split", "Consumption Type (vs)",  choices = hm2_split, selected = "General government consumption expenditure (GDP) NZDm(r) s.a.", multiple = FALSE),
    selectInput("hm2_split2", NULL,  choices = c("-", hm2_split), selected = "Private consumption expenditure (GDP) %(r) s.a.", multiple = FALSE)
  )
  ui_single(insert_inputs, p = "hm2_plot", h = "600px")
})

# hm3 Investment UI
ui_hm3 <- memoise(function() {
  hm3_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm3")))
  insert_inputs <- tagList(
    selectInput("hm3_split", "Investment Type",  choices = hm3_split, selected = hm3_split[1], multiple = FALSE),
    selectInput("hm3_split2", "Second Investment Type",  choices = c("-", hm3_split), selected = hm3_split[4], multiple = FALSE)
  )
  ui_single(insert_inputs, p = "hm3_plot", h = "600px")
})
# hm4 Domestic Trade UI
ui_hm4 <- memoise(function() {
  hm4_group <- filter_series(guide_rbnz, column = "Grouping", apply_filters = list(Graph = c("hm4")))
  hm4_dim <- filter_series(guide_rbnz, column = "Dim", apply_filters = list(Graph = c("hm4")))
  insert_inputs <- tagList(
    selectInput("hm4_dim", "Dimension",  choices = hm4_dim, selected = hm4_dim[1], multiple = FALSE),
    selectInput("hm4_group", "Trade Group",  choices = hm4_group, selected = hm4_group[1], multiple = FALSE)
  )
  ui_single(insert_inputs, p = "hm4_plot", h = "600px")
})

# hm5 GDP UI
ui_hm5 <- memoise(function() {
  hm5_names <- filter_series(guide_rbnz, column = "Names", apply_filters = list(Graph = c("hm5")))
  hm5_tier <- filter_series(guide_rbnz, column = c("Split", "Names"), apply_filters = list(Graph = c("hm5"))) |>
    group_by(Split) |>
    summarise(value = list(Names)) |>
    deframe()
  insert_inputs <- tagList(
    selectInput("hm5_names", "GDP Measures (Max 4)",  choices = hm5_tier, selected = c("GDP - Expenditure (Real $m s.a.)","GDP - Expenditure (Real y/y%)"), multiple = TRUE)
  )
  ui_single(insert_inputs, p = "hm5_plot", h = "600px")
})


# hm6 National Saving UI
ui_hm6 <- memoise(function() {
  hm6_names <- filter_series(guide_rbnz, column = "Names", apply_filters = list(Graph = c("hm6")))
  insert_inputs <- tagList(
    selectInput("hm6_names", "Saving Types",  choices = hm6_names, selected = hm6_names, multiple = TRUE)
  )
  ui_single(insert_inputs, p = "hm6_plot", h = "600px")
})

# hm7 Balance of Payments UI
ui_hm7 <- memoise(function() {
  hm7_group <- filter_series(guide_rbnz, column = "Grouping", apply_filters = list(Graph = c("hm7")))
  insert_inputs <- tagList(
    selectInput("hm7_group", "Balance Group",  choices = hm7_group, selected = hm7_group[1], multiple = FALSE)
  )
  ui_single(insert_inputs, p = "hm7_plot", h = "600px")
})
  
# hm8 Overseas Trade UI
ui_hm8 <- memoise(function() {
  hm8_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm8")))
  hm8_grouping <- filter_series(guide_rbnz, column = "Grouping", apply_filters = list(Graph = c("hm8")))
  insert_inputs <- tagList(
    selectInput("hm8_grouping", "Metric",  choices = hm8_grouping, selected = hm8_grouping[1], multiple = FALSE),
    checkboxGroupInput("hm8_split", "Trade Type",  choices = hm8_split, selected = hm8_split)
  )
  ui_single(insert_inputs, p = "hm8_plot", h = "600px")
})

# hm9 Labour Market UI
ui_hm9 <- memoise(function() {
  hm9_tier <- filter_series(guide_rbnz, column = c("Split", "Names", "Grouping"), apply_filters = list(Graph = "hm9")) |>
    group_by(Split) |>
    summarise(
      value = list(
        as.list(stats::setNames(Names, Grouping))
      ),
      .groups = "drop"
    ) |>
    deframe()
  insert_inputs <- tagList(
    selectInput("hm9_tier1", "Metric (max 4)",  choices = hm9_tier, selected = c("Labour force participation rate	 - % s.a.", "Labour cost index (LCI) - y/y%"), multiple = TRUE),
  )
  ui_single(insert_inputs, p = "hm9_plot", h = "600px")
})

# hm10 Housing UI
ui_hm10 <- memoise(function() {
  hm10_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm10")))
  insert_inputs <- tagList(
    selectInput("hm10_split_1", "Metric",  choices = hm10_split, selected = hm10_split[1], multiple = FALSE),
    selectInput("hm10_split_2", NULL,  choices = c("-", hm10_split), selected = hm10_split[2], multiple = FALSE)
  )
  ui_single(insert_inputs, p = "hm10_plot", h = "600px")
})

# hm14 Expectations UI
ui_hm14 <- memoise(function() {
  hm14_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hm14")))
  hm14_group <- filter_series(guide_rbnz, column = "Grouping", apply_filters = list(Graph = c("hm14")))
    hm14_tier <- filter_series(guide_rbnz, column = c("Split", "Names", "Grouping"), apply_filters = list(Graph = "hm14")) |>
    group_by(Split) |>
    summarise(
      value = list(
        as.list(stats::setNames(Names, Grouping))
      ),
      .groups = "drop"
    ) |>
    deframe()

  insert_inputs <- tagList(
    selectInput("hm14_tier", "Expectation Type",  choices = hm14_tier, selected = c("Annual GDP growth - 1 year out", "Annual CPI growth - 1 year out","Perception of monetary conditions (net) - 1 year out"), multiple = TRUE)
  )
  ui_single(insert_inputs, p = "hm14_plot", h = "600px")
})

# hs35 Retail Sales UI
ui_hc35 <- memoise(function() {
  hc35_split <- filter_series(guide_rbnz, column = "Split", apply_filters = list(Graph = c("hc35")))
  hc35_group <- filter_series(guide_rbnz, column = "Group", apply_filters = list(Graph = c("hc35")))
  insert_inputs <- tagList(
    selectInput("hc35_group", "Lending Group",  choices = hc35_group, selected = hc35_group[1], multiple = FALSE),
    checkboxGroupInput("hc35_split", "Lending",  choices = hc35_split, selected = hc35_split[c(1,3,4)])
  )
  ui_single(insert_inputs, p = "hc35_plot", h = "600px")
})

ui_fuel <- memoise(function() {
  insert_inputs <- tagList(
    selectInput("fuel_unit", "Fuel Unit",  choices = c("USD per Barrel", "NZD per Barrel"), selected = "USD per Barrel", multiple = FALSE)
  )
  ui_single(insert_inputs, p = "fuel_plot", h = "600px")
})

# accommodation data UI


ui_adp <- memoise(function() {
  if (exists("adp_propertytype", envir = cache)) {return(cache[["adp_propertytype"]])}
  else {
    adp_propertytype <- load_data("adpByRTO") %>% select(`PropertyType`) %>% unique()
    cache[["adp_propertytype"]] <- adp_propertytype
    adp_propertytype
  }
  if (exists("adp_region", envir = cache)) {return(cache[["adp_region"]])}
  else {
    adp_region <- load_data("adpByRTO") %>% select(Regions) %>% unique()
    adp_region <- adp_region[!adp_region$Region %in% c("New Zealand"), ]
    cache[["adp_region"]] <- adp_region
    adp_region
  }
  adp_metric <- filter_series(guide_rbnz, column = "Names", apply_filters = list(Graph = c("adp")))

  insert_inputs <- tagList(
    selectInput("adp_metric", "Metric",  choices = adp_metric, selected = adp_metric[1], multiple = FALSE),
    selectInput("adp_wrap", "Group",  choices = c("Group", "Region", "PropertyType"), selected = "Group", multiple = FALSE),
    selectInput("adp_region", "Region",  choices = adp_region, selected = "Total", multiple = TRUE),
    selectInput("adp_propertytype", "Property Type",  choices = adp_propertytype, selected = adp_propertytype, multiple = TRUE)
  )
  ui_single(insert_inputs, p = "adp_plot", h = "600px")
})

get_data_filters <- function(i, data) {
  if (exists(i, envir = cache)) {return(cache[[i]])}
  else {
    data_filters <- data %>% pull(!!sym(i)) %>% unique() %>% as.vector() %>% sort()
    cache[[i]] <- data_filters
    data_filters
  }
}


ui_border <- memoise(function() {
  border_split <- c("Residency/Country", "Overseas Port","New Zealand Port", "Passenger Type", "Travel Purpose")
  border_country <- get_data_filters("Residency/Country", load_data("border"))
  overseas_port <- get_data_filters("Overseas Port", load_data("border"))
  nz_port <- get_data_filters("New Zealand Port", load_data("border"))
  border_type <- get_data_filters("Passenger Type", load_data("border"))
  travel_purpose <- get_data_filters("Travel Purpose", load_data("border"))


  ui_border <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("border_split", "border_split",  choices =  border_split, selected = border_split[1], multiple = FALSE),
        selectInput("border_country", "border_country",  choices =  border_country, selected = c(), multiple = TRUE),
        selectInput("border_os_port", "border_os_port",  choices = overseas_port, selected = c(), multiple = TRUE),
        selectInput("border_nz_port", "border_nz_port",  choices = nz_port, selected = c(), multiple = TRUE),
        selectInput("border_passenger_type", "border_passenger_type",  choices = border_type, selected = c(), multiple = TRUE),
        selectInput("border_purpose", "border_purpose",  choices = travel_purpose, selected = c(), multiple = TRUE),
        style = "height: 100%;",
        width = 300
      ),
      card(
        class = "csv-card", full_screen = TRUE, plotlyOutput("border_plot",  height = "600px"),
        style = "height: 600px; max-width: 900px; width: 100%;"
      )
    ), style = "background-color: #EDF2F3 !important; height: 100%;"
  )
})

ui_bond <- memoise(function() {
  if (exists("bond_region", envir = cache)) {return(cache[["bond_region"]])}
  else {
    bond_location <- load_data("bond") %>% select(`Location`) %>% unique()
    cache[["bond_location"]] <- bond_location
    bond_location
  }
  bond_metric <- filter_series(guide_rbnz, column = "Names", apply_filters = list(Graph = c("bond")))
  insert_inputs <- tagList(
    selectInput("bond_metric", "Metric",  choices = bond_metric, selected = bond_metric[1], multiple = FALSE),
    selectInput("bond_location", "Location",  choices = bond_location, selected = c("ALL"), multiple = TRUE)
  )
  ui_bond <- ui_single(insert_inputs, p = "bond_plot", h = "600px")
})

ui_ect <- memoise(function() {
  ui_ect  <- fluidPage(
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar", position = "right",
        selectInput("fuel_unit", "Fuel Unit",  choices = c("USD per Barrel", "NZD per Barrel"), selected = "USD per Barrel", multiple = FALSE),
        style = "height: 100%;",
        width = 300
      ),
      card(
        class = "csv-card", full_screen = TRUE, plotlyOutput("fuel_plot",  height = "600px"),
        style = "height: 600px; max-width: 900px; width: 100%;"
      )
    ), style = "background-color: #EDF2F3 !important; height: 100%;"
  )
})
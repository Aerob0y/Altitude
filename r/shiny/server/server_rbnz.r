mod_hb2_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hb2_plot <- reactive({
      spec <- graph_specs$hb2
      valid_inputs <- input$hb2_tier |> unlist(use.names = FALSE) |> unique()
      if (length(valid_inputs) == 0) {
        valid_inputs <- c("Official Cash Rate (OCR)")
      }
      hb2_series <- filter_series(
        guide_rbnz,
        apply_filters = list(Graph = c("hb2"), Names = valid_inputs)
      ) |> prepare_series_for_graph(spec)

      prepared <- prepare_generic_plot_data(
        data = load_data("hb2"),
        series = hb2_series,
        k = "Date",
        graph_id = "hb2"
      )

      build_generic_plot(
        prepared,
        title = spec$title,
        subtitle = paste(spec$subtitle_prefix, short_title(valid_inputs)),
        yaxis_titles = spec$yaxis_titles,
        years = spec$years
      )
    })
    output$plot <- renderPlotly({hb2_plot()})
  })
}



#hm1 Prices
mod_hm1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hm1_plot <- reactive({
      generic_plotly(
        data = load_data("hm1"),
        titles = c("Prices", paste("RBNZ:", input$hm1_metric)),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm1"), Split = input$hm1_input[seq_len(min(length(input$hm1_input), 5))], Dim = input$hm1_metric)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({hm1_plot()})
  })
}

#hm2 Consumption
mod_hm2_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hm2_plot <- reactive({
      spec <- graph_specs$hm2
      valid_inputs <- unique(c(input$hm2_split, input$hm2_split2)) |> setdiff("-")
      hm2_series <- filter_series(
        guide_rbnz,
        apply_filters = list(Graph = c("hm2"), Split = valid_inputs)
      ) |> prepare_series_for_graph(spec)

      prepared <- prepare_generic_plot_data(
        data = load_data("hm2"),
        series = hm2_series,
        k = "Date",
        graph_id = "hm2"
      )

      build_generic_plot(
        prepared,
        title = spec$title,
        subtitle = paste(spec$subtitle_prefix, short_title(valid_inputs)),
        yaxis_titles = spec$yaxis_titles,
        years = spec$years
      )
    })
    output$plot <- renderPlotly({hm2_plot()})
  })
}

  #hm3 Investment
mod_hm3_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hm3_plot <- reactive({
      valid_inputs <- unique(c(input$hm3_split, input$hm3_split2)) |> setdiff("-")
      generic_plotly(
        data = load_data("hm3"),
        titles = c("Investment", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm3"), Split = valid_inputs)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({hm3_plot()})
  })
}

  #hm4 Domestic Trade
mod_hm4_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hm4_plot <- renderPlotly({
      generic_plotly(
        data = load_data("hm4"),
        titles = c("Domestic Trade", paste("RBNZ:", short_title(input$hm4_group))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm4"), Grouping = input$hm4_group)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({hm4_plot()})
  })
}
#hm5 Wages
mod_hm5_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hm5_plot <- renderPlotly({
      spec <- graph_specs$hm5
      valid_inputs <- input$hm5_names |> unlist(use.names = FALSE) |> unique()
      valid_inputs <- valid_inputs[seq_len(min(length(valid_inputs), spec$max_series))]
      hm5_series <- filter_series(
        guide_rbnz,
        apply_filters = list(Graph = c("hm5"), Names = valid_inputs)
      ) |> prepare_series_for_graph(spec)

      prepared <- prepare_generic_plot_data(
        data = load_data("hm5"),
        series = hm5_series,
        k = "Date",
        graph_id = "hm5"
      )

      build_generic_plot(
        prepared,
        title = spec$title,
        subtitle = paste(spec$subtitle_prefix, short_title(valid_inputs)),
        yaxis_titles = spec$yaxis_titles,
        years = spec$years
      )
    })
    output$plot <- renderPlotly({hm5_plot()})
  })
}

#hm6 Labour Market
mod_hm6_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hm6_plot <- renderPlotly({
      generic_plotly(
        data = load_data("hm6"),
        titles = c("National Saving", paste("RBNZ:", short_title(input$hm6_names))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm6"), Names = input$hm6_names)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({hm6_plot()})
  })
}

#hm7 Balance of Payments
mod_hm7_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hm7_plot <- renderPlotly({
      generic_plotly(
        data = load_data("hm7"),
        titles = c("Balance of Payments", paste("RBNZ:", input$hm7_group)),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm7"), Grouping = input$hm7_group)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({hm7_plot()})
  })
}
  #hm8 Government
mod_hm8_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hm8_plot <- renderPlotly({
      generic_plotly(
        data = load_data("hm8"),
        titles = c("Overseas Trade", paste("RBNZ:", short_title(unique(c(input$hm8_group1, input$hm8_group2))))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm8"), Split = input$hm8_split, Grouping = input$hm8_grouping)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({hm8_plot()})
  })
}

#hm9 National Saving
mod_hm9_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hm9_plot <- renderPlotly({
      x <- input$hm9_tier1
      if (length(x) >= 4) {x <- x[1:4]}
      if (length(x) == 0) {x <- c("Labour force participation rate	 - % s.a.", "Labour cost index (LCI) - y/y%")}
      generic_plotly(
        data = load_data("hm9"),
        titles = c("National Saving", paste("RBNZ:", short_title(x))),  
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm9"), Names = x)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({hm9_plot()})
  })
}

#hm10 Housing
mod_hm10_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    hm10_plot <- renderPlotly({
      valid_inputs <- unique(c(input$hm10_split_1, input$hm10_split_2)) |> setdiff("-")
      generic_plotly(
        data = load_data("hm10"),
        titles = c("Housing", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm10"), Split = valid_inputs)),
        k = "Date"
      )
    })
    output$plot <- renderPlotly({hm10_plot()})
  })
}
  #hm14 Construction
mod_hm3_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  output$hm14_plot <- renderPlotly({
    x <- input$hm14_tier
    if (length(x) >= 4) {x <- x[1:4]}
    if (length(x) == 0) {x <- c("Annual GDP growth - 1 year out", "Annual CPI growth - 1 year out","Perception of monetary conditions (net) - 1 year out")}
    generic_plotly(
      data = load_data("hm14"),
      titles = c("Perceptions and Expectations", ""),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm14"), Names = input$hm14_tier)),
      k = "Date"
    )
    })
    output$plot <- renderPlotly({hm3_plot()})
  })
}

  #hs35 Retail Sales
mod_hm3_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  output$hc35_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hc35"),
      titles = c("Residential mortgage loan reconciliation", paste("RBNZ:", short_title(unique(c(input$hc35_group, input$hc35_split))))),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hc35"), Split = input$hc35_split, Group = input$hc35_group)),
      k = "Date"
    )
    })
    output$plot <- renderPlotly({hm3_plot()})
  })
}

  #Fuel
mod_hm3_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  output$fuel_plot <- renderPlotly({
    generic_plotly(
      data = load_data("fuel"),
      titles = c("Jet A1 Fuel Prices per Barrel", paste("In", input$fuel_unit)),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("fuel"), Split = input$fuel_unit)),
      k = "Date"
    )
    })
    output$plot <- renderPlotly({hm3_plot()})
  })
}
  #Bond 
mod_hm3_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  output$bond_plot <- renderPlotly({
    data <- load_data("bond")
    data <- data %>% filter(Location %in% input$bond_location)
    plot_long(
      data = data,
      titles = c("Bond Prices", ""),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("bond"), Names = input$bond_metric)),
      k = "Date",
      split = "Location"
    )
    })
    output$plot <- renderPlotly({hm3_plot()})
  })
}

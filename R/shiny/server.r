
server <- function(input, output) {
  #hb1 Exchange rates
  output$hb1_plot <- renderPlotly({
    valid_inputs <- unique(c(input$Currency1, input$Currency2)) |> setdiff("-")
    generic_plotly(
      data = load_data("hb1"),
      t1 = "Daily exchange rates and TWI",
      t2 =  paste("RBNZ:", paste(valid_inputs, collapse = " and ")),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hb1"), Split = valid_inputs)),
      k = "Date"
    )
  })
  #hb2 Interest rates
  output$hb2_plot <- renderPlotly({
    valid_inputs <- input$hb2_tier |> unlist(use.names = FALSE) |> unique()
    generic_plotly(
      data = load_data("hb2"),
      t1 = "Daily wholesale interest rates",
      t2 =  paste("RBNZ:", paste(valid_inputs, collapse = " and ")),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hb2"), Names = valid_inputs)),
      k = "Date"
    )
  })
  #hm1 Prices
  output$hm1_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hm1"),
      t1 = "Prices",
      t2 =  paste("RBNZ:", input$hm1_metric),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm1"), Split = input$hm1_input, Dim = input$hm1_metric)),
      k = "Date"
    )
  })
  #hm2 Consumption
  output$hm2_plot <- renderPlotly({
    valid_inputs <- unique(c(input$hm2_split, input$hm2_split2)) |> setdiff("-")
    generic_plotly(
      data = load_data("hm2"),
      t1 = "Consumption",
      t2 =  paste("RBNZ:", paste(valid_inputs, collapse = " and ")),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm2"), Split = valid_inputs)),
      k = "Date"
    )
  })
  #hm3 Investment
  output$hm3_plot <- renderPlotly({
    valid_inputs <- unique(c(input$hm3_split, input$hm3_split2)) |> setdiff("-")
    generic_plotly(
      data = load_data("hm3"),
      t1 = "Investment",
      t2 =  paste("RBNZ:", paste(valid_inputs, collapse = " and ")),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm3"), Split = valid_inputs)),
      k = "Date"
    )
  })
  #hm4 Domestic Trade
  output$hm4_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hm4"),
      t1 = "Domestic Trade",
      t2 =  paste("RBNZ:", input$hm4_group),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm4"), Group = input$hm4_group)),
      k = "Date"
    )
  })
  #hm5 Wages
  output$hm5_plot <- renderPlotly({
    valid_inputs <- input$hm5_names |> unlist(use.names = FALSE) |> unique()
    generic_plotly(
      data = load_data("hm5"),
      t1 = "GDP",
      t2 =  paste("RBNZ:", paste(valid_inputs, collapse = " and ")),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm5"), Names = valid_inputs)),
      k = "Date"
    )
  })
  #hm6 Labour Market
  output$hm6_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hm6"),
      t1 = "National Saving",
      t2 =  paste("RBNZ:", paste(input$hm6_names, collapse = " and ")),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm6"), Names = input$hm6_names)),
      k = "Date"
    )
  })
  #hm7 Balance of Payments
  output$hm7_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hm7"),
      t1 = "Balance of Payments",
      t2 =  paste("RBNZ:", input$hm7_group),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm7"), Group = input$hm7_group)),
      k = "Date"
    )
  })
  #hm8 Government
  output$hm8_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hm8"),
      t1 = "Overseas Trade",
      t2 =  paste("RBNZ:", paste(unique(c(input$hm8_group1, input$hm8_group2)), collapse = " and ")),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm8"), Split = input$hm8_split, Dim = input$hm8_dim)),
      k = "Date"
    )
  })
  #hm9 National Saving
  output$hm9_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hm9"),
      t1 = "National Saving",
      t2 =  paste("RBNZ:", paste(input$hm9_names, collapse = " and ")),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm9"), Adj = input$hm9_adj, Split = input$hm9_split)),
      k = "Date"
    )
  })
  #hm10 Housing
  output$hm10_plot <- renderPlotly({
    valid_inputs <- unique(c(input$hm10_split_1, input$hm10_split_2)) |> setdiff("-")
    generic_plotly(
      data = load_data("hm10"),
      t1 = "Housing",
      t2 =  paste("RBNZ:", paste(valid_inputs, collapse = " and ")),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm10"), Split = valid_inputs)),
      k = "Date"
    )
  })
  #hm14 Construction
  output$hm14_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hm14"),
      t1 = "Construction",
      t2 =  paste("RBNZ:", paste(unique(c(input$hm14_dim, input$hm14_split)), collapse = " and ")),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm14"), Dim = input$hm14_dim, Split = input$hm14_split)),
      k = "Date"
    )
  })

  #hs35 Retail Sales
  output$hc35_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hc35"),
      t1 = "Residential mortgage loan reconciliation",
      t2 =  paste("RBNZ:", paste(unique(c(input$hc35_group, input$hc35_split)), collapse = " and ")),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hc35"), Split = input$hc35_split)),
      k = "Date"
    )
  })
  #hs32 Loans
  output$hs32_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hs32"),
      t1 = "Loans",
      t2 =  paste("RBNZ:", paste(unique(c(input$hs32_adj, input$hs32_group)), collapse = " and ")),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hs32"), Adj = input$hs32_adj, Group = input$hs32_group)),
      k = "Date"
    )
  })
}

load_data("hm1")

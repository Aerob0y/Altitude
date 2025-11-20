
print(Sys.time() - time_start)
time_start <- Sys.time()

server <- function(input, output) {
  output$hb1_plotx <- renderPlotly({
    x <-   if (input$Currency1 == "-" && input$Currency2 == "-") {paste(input$Currency1,input$Currency2, sep = " & ")
    } else if (input$Currency1 != "-" && input$Currency2 == "-" || input$Currency1 == input$Currency2) {paste(input$Currency1)
    } else if (input$Currency1 == "-" && input$Currency2 != "-") {paste(input$Currency2)
    } else {paste(input$Currency1, "and", input$Currency2)
    }
    x <- paste("Daily exchange rates: ", x)
    rbnz_plotly(title = "Exchange rates", subtitle = x, g = "hb1", split = c(input$Currency1, input$Currency2))
  })
  output$hb2_plotx <- renderPlotly({
    #rbnz_plotly(title = "Daily Wholesale Interest Rates", subtitle = paste("RBNZ:", input$hb2_group), g = "hb2", split = input$hb2_split, group = input$hb2_group,
    #  split_fallback = c("Official Cash Rate (OCR)"), group_fallback = c("OCR")
    #)
    x <- input$hb2_tier %>% unlist(.hb2_tier, use.names = FALSE) %>% unique()
    y <- .hb2_tier$'Swap rates close' %>% unlist(use.names = FALSE) %>% unique()
    rbnz_plotly(title = "Daily Wholesale Interest Rates", g = "hb2", names = x, names_fallback = y)
  })

  output$hm1_plotx <- renderPlotly({
    rbnz_plotly(title = "Prices", subtitle = paste("RBNZ:", input$hm1_metric), g = "hm1", split = input$hm1_input, dim = input$hm1_metric)
  })


  output$hm2_plotx <- renderPlotly({
    x <- if(input$hm2_split2 == "-") {
      c(input$hm2_split)
    } else {
      c(input$hm2_split, input$hm2_split2)
    }
    rbnz_plotly(title = "Consumption", subtitle = paste("RBNZ:", paste(x, collapse = " and ")), g = "hm2", split = x)
  })

  #HM3 Investment
  output$hm3_plotx <- renderPlotly({
    x <- if(input$hm3_split2 == "-") {
      c(input$hm3_split)
    } else {
      c(input$hm3_split, input$hm3_split2)
    }

    a <- rbnz_series(col = 'Grouping', split = input$hm3_split, filter_graph = 'hm3')
    b <- rbnz_series(col = 'Grouping', split = input$hm3_split2, filter_graph = 'hm3')
    y <- unique(c(a, b))
    rbnz_plotly(title = "Investment", subtitle = paste("RBNZ:", paste(y, collapse = " and ")), g = "hm3", split = x)
  })


  #dim = input$hm4_dim, split = input$hm4_split
  output$hm4_plotx <- renderPlotly({
    rbnz_plotly(title = "Domestic Trade", subtitle = paste("RBNZ: ", input$hm4_group), g = "hm4", group = input$hm4_group)
  })

  output$hm5_plotx <- renderPlotly({
    #if (length(input$hm5_names[1]) == 0) {
    #  print("X")
    #  x <- .cache$series %>% filter(Graph == "hm5") %>%
    #  filter((Adj == input$hm5_dim1 & Split == input$hm5_split1) | (Adj == input$hm5_dim2 & Split == input$hm5_split2)) %>% 
    #  select(Names) %>% unique() %>% unlist() %>% as.vector()
    #} else {
    #  print("Y")
    #  x <- input$hm5_names %>% unique() %>% unlist() %>% as.vector()
    #}
    x <- if (length(input$hm5_names) <= 2) {
      paste(input$hm5_names, collapse = " & ")
    } else {
        "Various Measures"
    }
    rbnz_plotly(title = "GDP", subtitle = x, g = "hm5", names = input$hm5_names, names_fallback = 'Export of Goods & Services (Nominal $m)')
  })
  output$hm6_plotx <- renderPlotly({
    rbnz_plotly(title = "National Saving", subtitle = "RBNZ: Savings by Group", g = "hm6", names = input$hm6_names)
  })
  output$hm7_plotx <- renderPlotly({
    rbnz_plotly(title = "Balance of Payments", subtitle = "Subtitle", g = "hm7", group = input$hm7_group)
  })
  output$hm8_plotx <- renderPlotly({
    x <- if(input$hm8_group2 == "-") {
      c("Export", "Import")
    } else {
      input$hm8_group2
    }
    rbnz_plotly(title = "Overseas Trade", subtitle = paste(unique(c(input$hm8_group1, input$hm8_group2), collapse = " & ")), g = "hm8", split = x, group = c(input$hm8_group1, input$hm8_group2))
  })
  output$hm9_plotx <- renderPlotly({
    rbnz_plotly(title = "Labour Market", subtitle = "Subtitle", g = "hm9", adj = input$hm9_adj, split = input$hm9_split)
  })
  output$hm10_plotx <- renderPlotly({
    rbnz_plotly(title = "Housing", subtitle = paste("RBNZ: ", paste(unique(c(input$hm10_split_1, input$hm10_split_2)), collapse = " & ")), g = "hm10", split = c(input$hm10_split_1, input$hm10_split_2)) 
  })

  output$hm14_plotx <- renderPlotly({
    x <- if (length(input$hm14_split) <= 2) {
      paste0(input$hm14_split, collapse = " & ")
    } else if (length(input$hm14_group) <= 2) {
       paste0(input$hm14_group, collapse = " & ")
    } else {
      "Perceptions of the Economy"
    }
    #rbnz_plotly(title = "Business Expectations", subtitle = x, g = "hm14", split = input$hm14_split, group = input$hm14_group)
    rbnz_plotly(title = "Business Expectations", subtitle = x, g = "hm14", split = input$hm14_split, group = input$hm14_group, group_fallback = c("1 year out", "2 years out","5 years out"))
  })

  output$hs32_plotx <- renderPlotly({
    switch(input$hs32_adj,
      "Total" = {
        rbnz_plotly(title = "Loans", subtitle = "Lending by group", g = "hs32", adj = c("Total"))
      },
      "Use" = {
        rbnz_plotly(title = "Loans", subtitle = "Lending by group", g = "hs32", split = input$hs32_split)
      },
      "Type" = {
        rbnz_plotly(title = "Loans", subtitle = "Lending by group", g = "hs32", group = input$hs32_group)
      }
    )
  })
  output$hc35_plotx <- renderPlotly({
    switch(input$hc35_adj,
      "Total" = {
        rbnz_plotly(title = "Residential mortgage loan reconciliation", subtitle = "Lending by group", g = "hc35", adj = c("Total"))
      },
      "Use" = {
        rbnz_plotly(title = "Residential mortgage loan reconciliation", subtitle = "Lending by group", g = "hc35", split = input$hc35_split)
      },
      "Type" = {
        rbnz_plotly(title = "Residential mortgage loan reconciliation", subtitle = "Lending by group", g = "hc35", group = input$hc35_group)
      }
    )
  })
  output$fuel_plot <- renderPlotly({
    fuel_plotly()
  })

}
print(Sys.time() - time_start)
time_start <- Sys.time()



server <- function(input, output) {
  output$hb1_plotx <- renderPlotly({
    x <-   if (input$Currency1 == "-" && input$Currency2 == "-") {paste(input$Currency1,input$Currency2, sep = " & ")
    } else if (input$Currency1 != "-" && input$Currency2 == "-") {paste(input$Currency1)
    } else if (input$Currency1 == "-" && input$Currency2 != "-") {paste(input$Currency2)
    } else {paste(input$Currency1, "and", input$Currency2)
    }
    x <- paste("Daily exchange rates: ", x)
    rbnz_plotly(title = "Exchange rates", subtitle = x, g = "hb1", split = c(input$Currency1, input$Currency2))
  })
  output$hb2_plotx <- renderPlotly({
    rbnz_plotly(title = "Daily wholesale interest rates", subtitle = "Subtitle", g = "hb2", split = input$hb2_input)
  })
  output$hm1_plotx <- renderPlotly({
    rbnz_plotly(title = "consumer price index (CPI)", subtitle = "Subtitle", g = "hm1", split = input$hm1_input, dim = input$hm1_metric)
  })

  output$hm2_plotx <- renderPlotly({
    rbnz_plotly(title = "Prices", subtitle = "Subtitle", g = "hm2", split = input$hm2_split, dim = input$hm2_dim)
  })
  output$hm3_plotx <- renderPlotly({
    rbnz_plotly(title = "Investment", subtitle = "Subtitle", g = "hm3", split = input$hm3_split, dim = input$hm3_dim)
  })
  output$hm4_plotx <- renderPlotly({
    rbnz_plotly(title = "Domestic Trade", subtitle = "Subtitle", g = "hm4", dim = input$hm4_dim, split = input$hm4_split, group = input$hm4_group)
  })
  output$hm5_plotx <- renderPlotly({
    rbnz_plotly(title = "GDP", subtitle = "Subtitle", g = "hm5", dim = input$hm5_dim, split = input$hm5_split)
  })
  output$hm6_plotx <- renderPlotly({
    rbnz_plotly(title = "National Saving", subtitle = "Subtitle", g = "hm6")
  })
  output$hm7_plotx <- renderPlotly({
    rbnz_plotly(title = "Balance of Payments", subtitle = "Subtitle", g = "hm7")
  })
  output$hm8_plotx <- renderPlotly({
    rbnz_plotly(title = "Overseas Trade", subtitle = "Subtitle", g = "hm8")
  })
  output$hm9_plotx <- renderPlotly({
    rbnz_plotly(title = "Labour Market", subtitle = "Subtitle", g = "hm9")
  })
  output$hm10_plotx <- renderPlotly({
    rbnz_plotly(title = "Housing", subtitle = "Subtitle", g = "hm10")
  })
  output$hm14_plotx <- renderPlotly({
    rbnz_plotly(title = "Expectations", subtitle = "Subtitle", g = "hm14")
  })
  output$hs32_plotx <- renderPlotly({
    rbnz_plotly(title = "Loans", subtitle = "Subtitle", g = "hc32")
  })
  output$hc35_plotx <- renderPlotly({
    rbnz_plotly(title = "Residential mortgage loan reconciliation", subtitle = "Subtitle", g = "hc35")
  })

}

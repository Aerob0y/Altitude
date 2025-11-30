
server <- function(input, output) {
  #output$hb1_ui    <- renderUI({ui_hb1_main()})
  output$hc35_ui   <- renderUI({ui_hc35()})
  output$hm1_ui    <- renderUI({ui_hm1()})
  output$hm2_ui    <- renderUI({ui_hm2()})
  output$hm3_ui    <- renderUI({ui_hm3()})
  output$hm4_ui    <- renderUI({ui_hm4()})
  output$hm5_ui    <- renderUI({ui_hm5()})
  output$hm6_ui    <- renderUI({ui_hm6()})
  output$hm7_ui    <- renderUI({ui_hm7()})
  output$hm8_ui    <- renderUI({ui_hm8()})
  output$hm9_ui    <- renderUI({ui_hm9()})
  output$hm10_ui   <- renderUI({ui_hm10()})
  output$hm14_ui   <- renderUI({ui_hm14()})
  output$hs32_ui   <- renderUI({ui_hs32()})
  output$hc35_ui   <- renderUI({ui_hc35()})
  output$fuel_ui   <- renderUI({ui_fuel()})
  output$adp_ui    <- renderUI({ui_adp()})
  output$bond_ui   <- renderUI({ui_bond()})
  output$ect_ui    <- renderUI({ui_ect()})
  output$border_ui <- renderUI({ui_border()})
  output$slides_ui <- renderUI({ui_slides()})


  #hb1 Exchange rates
hb1_plot_obj <- function(a, b) {
  valid_inputs <- unique(c(a, b)) |> setdiff("-")

  generic_plotly(
    data   = load_data("hb1"),
    titles = c(
      "Daily exchange rates and TWI",
      paste("RBNZ:", short_title(valid_inputs))
    ),
    series = filter_series(
      guide_rbnz,
      apply_filters = list(Graph = "hb1", Split = valid_inputs)
    ),
    k = "Date"
  )
}



# First plot (driven by _main inputs)
output$hb1_plot_main <- renderPlotly({
  hb1_plot_obj(input$Currency1_main, input$Currency2_main)
})

# Second plot (driven by _alt inputs)
output$hb1_plot_main2 <- renderPlotly({
  hb1_plot_obj(input$Currency1_alt, input$Currency2_alt)
})

    output$hb1_plot <-
    renderPlotly({
      valid_inputs <- unique(c(input$Currency1, input$Currency2)) |> setdiff("-")
      generic_plotly(
        data = load_data("hb1"),
        titles = c("Daily exchange rates and TWI", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hb1"), Split = valid_inputs)),
        k = "Date"
      )
    })
  
 # output$hb1_plot <- hb1_plot_obj()



  #hb2 Interest rates
  output$hb2_plot <- renderPlotly({
    valid_inputs <- input$hb2_tier |> unlist(use.names = FALSE) |> unique()
    if (length(valid_inputs) == 0) {
      valid_inputs <- c("Official Cash Rate (OCR)")
    }
      generic_plotly(
        data = load_data("hb2"),
        titles = c("Daily wholesale interest rates", paste("RBNZ:", short_title(valid_inputs))),
        series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hb2"), Names = valid_inputs)),
        k = "Date",
        years = 15
      )
  })

  # hb2 module server ----
mod_hb2_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    hb2_plot <- reactive({
      valid_inputs <- input$hb2_tier |> unlist(use.names = FALSE) |> unique()
    if (length(valid_inputs) == 0) {
      valid_inputs <- c("Official Cash Rate (OCR)")
    }

      generic_plotly(
      data = load_data("hb2"),
      titles = c("Daily wholesale interest rates", paste("RBNZ:", short_title(valid_inputs))),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hb2"), Names = valid_inputs)),
      k = "Date",
      years = 15
    )
    })

    output$plot <- renderPlotly({
      hb2_plot()
    })

    # optional, if you ever want the plot object outside:
    return(hb2_plot)
  })
}
mod_hb2_server("hb2_main")
mod_hb2_server("hb2_alt")









  #hm1 Prices
  output$hm1_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hm1"),
      titles = c("Prices", paste("RBNZ:", input$hm1_metric)),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm1"), Split = input$hm1_input[seq_len(min(length(input$hm1_input), 5))], Dim = input$hm1_metric)),
      k = "Date"
    )
  })
  #hm2 Consumption
  output$hm2_plot <- renderPlotly({
    valid_inputs <- unique(c(input$hm2_split, input$hm2_split2)) |> setdiff("-")
    generic_plotly(
      data = load_data("hm2"),
      titles = c("Consumption", paste("RBNZ:", short_title(valid_inputs))),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm2"), Split = valid_inputs)),
      k = "Date"
    )
  })
  #hm3 Investment
  output$hm3_plot <- renderPlotly({
    valid_inputs <- unique(c(input$hm3_split, input$hm3_split2)) |> setdiff("-")
    generic_plotly(
      data = load_data("hm3"),
      titles = c("Investment", paste("RBNZ:", short_title(valid_inputs))),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm3"), Split = valid_inputs)),
      k = "Date"
    )
  })
  #hm4 Domestic Trade
  output$hm4_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hm4"),
      titles = c("Domestic Trade", paste("RBNZ:", short_title(input$hm4_group))),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm4"), Grouping = input$hm4_group)),
      k = "Date"
    )
  })
  #hm5 Wages
  output$hm5_plot <- renderPlotly({
    valid_inputs <- input$hm5_names |> unlist(use.names = FALSE) |> unique()
    valid_inputs <- valid_inputs[seq_len(min(length(valid_inputs), 4))]
    generic_plotly(
      data = load_data("hm5"),
      titles = c("GDP", paste("RBNZ:", short_title(valid_inputs))),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm5"), Names = valid_inputs)),
      k = "Date"
    )
  })
  #hm6 Labour Market
  output$hm6_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hm6"),
      titles = c("National Saving", paste("RBNZ:", short_title(input$hm6_names))),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm6"), Names = input$hm6_names)),
      k = "Date"
    )
  })
  #hm7 Balance of Payments
  output$hm7_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hm7"),
      titles = c("Balance of Payments", paste("RBNZ:", input$hm7_group)),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm7"), Grouping = input$hm7_group)),
      k = "Date"
    )
  })
  #hm8 Government
  output$hm8_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hm8"),
      titles = c("Overseas Trade", paste("RBNZ:", short_title(unique(c(input$hm8_group1, input$hm8_group2))))),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm8"), Split = input$hm8_split, Grouping = input$hm8_grouping)),
      k = "Date"
    )
  })
  
  #hm9 National Saving
  output$hm9_plot <- renderPlotly({
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
  #hm10 Housing
  output$hm10_plot <- renderPlotly({
    valid_inputs <- unique(c(input$hm10_split_1, input$hm10_split_2)) |> setdiff("-")
    generic_plotly(
      data = load_data("hm10"),
      titles = c("Housing", paste("RBNZ:", short_title(valid_inputs))),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hm10"), Split = valid_inputs)),
      k = "Date"
    )
  })
  #hm14 Construction
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

  #hs35 Retail Sales
  output$hc35_plot <- renderPlotly({
    generic_plotly(
      data = load_data("hc35"),
      titles = c("Residential mortgage loan reconciliation", paste("RBNZ:", short_title(unique(c(input$hc35_group, input$hc35_split))))),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("hc35"), Split = input$hc35_split, Group = input$hc35_group)),
      k = "Date"
    )
  })

  #Fuel
  output$fuel_plot <- renderPlotly({
    generic_plotly(
      data = load_data("fuel"),
      titles = c("Jet A1 Fuel Prices per Barrel", paste("In", input$fuel_unit)),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("fuel"), Split = input$fuel_unit)),
      k = "Date"
    )
  })

  #Bond 
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

  #Bond
  output$border_plot <- renderPlotly({
    data <- load_data("border")
    if(length(input$border_country) > 0) {
      data <- data %>% filter(`Residency/Country` %in% input$border_country)
    }
    if(length(input$border_os_port) > 0) {
      data <- data %>% filter(`Overseas Port` %in% input$border_os_port)
    }
    if(length(input$border_nz_port) > 0) {
      data <- data %>% filter(`New Zealand Port` %in% input$border_nz_port)
    }
    if(length(input$border_passenger_type) > 0) {
      data <- data %>% filter(`Passenger Type` %in% input$border_passenger_type)
    }
    if(length(input$border_purpose) > 0) {
      data <- data %>% filter(`Travel Purpose` %in% input$border_purpose)
    }
    data <- data %>%
      group_by(Date, !!sym(input$border_split[1])) %>%
      summarise(TotalPassengers = sum(TotalPassengers, na.rm = TRUE), .groups = "drop", quiet = TRUE) %>%
      mutate(Date = as.Date(Date)) %>%
      ungroup() %>%
      arrange(Date)
    
    plot_long(
      data = data,
      titles = c("Arriving Passengers", ""),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("border"), Names = c("TotalPassengers"))),
      k = "Date",
      split = input$border_split[1]
    )
  })


    #Fuel
  output$adp_plot2 <- renderPlotly({
    input_adp_region <- input$adp_region
    if (is.null(input$adp_region) || length(input$adp_region) == 0) {
      input_adp_region <- "New Zealand"
    }
    input_adp_propertytype <- input$adp_propertytype
    if (is.null(input$adp_propertytype) || length(input$adp_propertytype) == 0) {
      input_adp_propertytype <- "Total"
    }

    data_grouped <- load_adp() %>%
      filter(Regions %in% input_adp_region) %>%
      filter(PropertyType %in% input_adp_propertytype)

    data_grouped <- data_grouped %>%
      mutate(Date = as.Date(Date))


    switch(input$adp_wrap[1],
      "Group" = {
        data_grouped$Regions <- paste(unique(data_grouped$Regions), collapse = ",")
        data_grouped$PropertyType <- paste(unique(data_grouped$PropertyType), collapse = ",")
        m <- "Regions"
      },
      "Region" = {
        data_grouped$PropertyType <- paste(unique(data_grouped$PropertyType), collapse = ",")
        m <- "Regions"
      },
      "PropertyType" = {
        data_grouped$Regions <- paste(unique(data_grouped$Regions), collapse = ",")
        m <- "PropertyType"
      }
    )
    
    data_grouped <- data_grouped %>% group_by(Date, Regions, PropertyType)
    data_grouped <- data_grouped %>%
      summarise(
        `Available monthly stay unit capacity` = sum(`Available monthly stay unit capacity`, na.rm = TRUE),
        `Domestic guest nights` = sum(`Domestic guest nights`, na.rm = TRUE),
        `Guest arrivals` = sum(`Guest arrivals`, na.rm = TRUE),
        `International guest nights` = sum(`International guest nights`, na.rm = TRUE),
        `Monthly stay unit capacity` = sum(`Monthly stay unit capacity`, na.rm = TRUE),
        `Number of active establishments` = sum(`Number of active establishments`, na.rm = TRUE),
        `Number of establishments` = sum(`Number of establishments`, na.rm = TRUE),
        `Number of stay units` = sum(`Number of stay units`, na.rm = TRUE),
        `Stay unit nights occupied` = sum(`Stay unit nights occupied`, na.rm = TRUE),
        `Total guest nights` = sum(`Total guest nights`, na.rm = TRUE)
      ) %>%
      mutate(
        `Average guests per stay unit night` = `Total guest nights` / `Stay unit nights occupied`,
        `Average nights stayed per guest` = `Total guest nights` / `Guest arrivals`,
        `Average stay units per establishment` = `Number of stay units` / `Number of establishments`,
        `Capacity utilisation rate (%)` = (`Stay unit nights occupied` / `Monthly stay unit capacity`) * 100,
        `Occupancy rate (%)` = (`Stay unit nights occupied` / `Available monthly stay unit capacity`) * 100,
        `Percentage of stay unit capacity available (%)` = (`Available monthly stay unit capacity` / `Monthly stay unit capacity`) * 100,
        `Proportion of domestic guests (%)` = sum(`Domestic guest nights`, na.rm = TRUE) / sum(`Total guest nights`, na.rm = TRUE) * 100,
        `Proportion of international guests (%)` = sum(`International guest nights`, na.rm = TRUE) / sum(`Total guest nights`, na.rm = TRUE) * 100
      ) %>%
      ungroup() %>%
      arrange(Date)

    region_list <- ""
    propertytype_list <- ""

    plot_ts_by_region(
      data       = data_grouped,
      date_col   = "Date",
      region_col = m,
      value_col  = input$adp_metric,
      titles      = c("Accommodation Data", paste("ADP:", short_title(input$adp_metric))),
      subtitle   = "Monthly"
    )
  })

output$adp_plot <- renderPlotly({
    input_adp_region <- input$adp_region
    if (is.null(input$adp_region) || length(input$adp_region) == 0) {
      input_adp_region <- "New Zealand"
    }
    input_adp_propertytype <- input$adp_propertytype
    if (is.null(input$adp_propertytype) || length(input$adp_propertytype) == 0) {
      input_adp_propertytype <- "Total"
    }

    data_grouped <- load_adp() %>%
      filter(Regions %in% input_adp_region) %>%
      filter(PropertyType %in% input_adp_propertytype)

    data_grouped <- data_grouped %>%
      mutate(Date = as.Date(Date))


    switch(input$adp_wrap[1],
      "Group" = {
        data_grouped$Regions <- paste(unique(data_grouped$Regions), collapse = ",")
        data_grouped$PropertyType <- paste(unique(data_grouped$PropertyType), collapse = ",")
        m <- "Regions"
      },
      "Region" = {
        data_grouped$PropertyType <- paste(unique(data_grouped$PropertyType), collapse = ",")
        m <- "Regions"
      },
      "PropertyType" = {
        data_grouped$Regions <- paste(unique(data_grouped$Regions), collapse = ",")
        m <- "PropertyType"
      }
    )
    data_grouped <- data_grouped %>% group_by(Date, Regions, PropertyType)
    data_grouped <- data_grouped %>%
      summarise(
        `Available monthly stay unit capacity` = sum(`Available monthly stay unit capacity`, na.rm = TRUE),
        `Domestic guest nights` = sum(`Domestic guest nights`, na.rm = TRUE),
        `Guest arrivals` = sum(`Guest arrivals`, na.rm = TRUE),
        `International guest nights` = sum(`International guest nights`, na.rm = TRUE),
        `Monthly stay unit capacity` = sum(`Monthly stay unit capacity`, na.rm = TRUE),
        `Number of active establishments` = sum(`Number of active establishments`, na.rm = TRUE),
        `Number of establishments` = sum(`Number of establishments`, na.rm = TRUE),
        `Number of stay units` = sum(`Number of stay units`, na.rm = TRUE),
        `Stay unit nights occupied` = sum(`Stay unit nights occupied`, na.rm = TRUE),
        `Total guest nights` = sum(`Total guest nights`, na.rm = TRUE)
      ) %>%
      mutate(
        `Average guests per stay unit night` = `Total guest nights` / `Stay unit nights occupied`,
        `Average nights stayed per guest` = `Total guest nights` / `Guest arrivals`,
        `Average stay units per establishment` = `Number of stay units` / `Number of establishments`,
        `Capacity utilisation rate (%)` = (`Stay unit nights occupied` / `Monthly stay unit capacity`) * 100,
        `Occupancy rate (%)` = (`Stay unit nights occupied` / `Available monthly stay unit capacity`) * 100,
        `Percentage of stay unit capacity available (%)` = (`Available monthly stay unit capacity` / `Monthly stay unit capacity`) * 100,
        `Proportion of domestic guests (%)` = sum(`Domestic guest nights`, na.rm = TRUE) / sum(`Total guest nights`, na.rm = TRUE) * 100,
        `Proportion of international guests (%)` = sum(`International guest nights`, na.rm = TRUE) / sum(`Total guest nights`, na.rm = TRUE) * 100
      ) %>%
      ungroup() %>%
      arrange(Date)

    region_list <- ""
    propertytype_list <- ""

    #generic_plotly2(
    #  data       = data_grouped,
    #  k   = "Date",
    #  split = m,
    #  shape = "long",
    #  series  = filter_series(guide_rbnz, apply_filters = list(Graph = c("adp"), Names = input$adp_metric)),
    #  titles      = c("Accommodation Data", paste("ADP:", short_title(input$adp_metric))),
    #)
    plot_long(
      data = data_grouped,
      titles = c("Accommodation Data", paste("ADP:", short_title(input$adp_metric))),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("adp"), Names = input$adp_metric)),
      k = "Date",
      split = m
    )

  })

















  n_slides <- 3
  slide <- reactiveVal(1)
  observeEvent(input$n, {
    slide(ifelse(slide() == n_slides, 1, slide() + 1) )   # wrap around
  })
  observeEvent(input$p, {
    slide(ifelse(slide() == 1, n_slides, slide() - 1) )   # wrap around
  })


  # Slide content (plots + text) per slide
  output$slide_body <- renderUI({
    s <- slide()
    if (s == 1) {
      tagList(
        div(class = "slide-subtitle", "Globally, central banks have increased interest rates, cooling the economy to battle inflation.  The last mile is the hardest, the inflation battle is not yet over."),
        fluidRow(
          column(7, ui_hb1_alt()),
          column(5, mod_hb2_ui("hb2_alt"))
        ),
        br(),
        div(class = "slide-text",
            tags$strong("Inflation busting"), tags$br(),
            tags$ul(
              tags$li("Post-COVID, inflation surged and central banks hiked policy rates."),
              tags$li("Higher rates cooled demand and helped bring inflation down."),
              tags$li("The last mile back to target is taking longer.")
            )
        )
      )
    } else if (s == 2) {
      tagList(
        div(class = "slide-subtitle", "Labour market"),
        fluidRow(
          column(8, "XX"),
          column(4,
                 div(class = "slide-text",
                     tags$ul(
                       tags$li("Job growth has slowed from its peak."),
                       tags$li("Wage growth has rolled over but remains elevated."),
                       tags$li("Unemployment is drifting higher from very low levels.")
                     )
                 )
          )
        )
      )
    } else if (s == 3) {
      tagList(
        div(class = "slide-subtitle", "Housing & construction"),
        fluidRow(
          column(8, "XX"),
          column(4,
                 div(class = "slide-text",
                     tags$ul(
                       tags$li("House prices have stabilised after the correction."),
                       tags$li("Construction activity is easing from very high levels."),
                       tags$li("Higher rates and costs continue to weigh on new builds.")
                     )
                 )
          )
        )
      )
    }
  })
  






}

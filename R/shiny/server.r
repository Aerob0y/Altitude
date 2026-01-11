
register_modules <- function(tabs) {
  #mod_hb1_server("hb1_main", enabled = reactive(TRUE))
  mod_hb1_server("hb1_main", selected_tab = tabs)

  mod_hb2_server("hb2_main", enabled = reactive(TRUE))
  mod_hb2_server("hb2_alt", enabled = reactive(TRUE))
  mod_hb2_server("hb2_a", enabled = reactive(TRUE))
  mod_hb2_server("hb2_b", enabled = reactive(TRUE))
  mod_hm1_server("hm1", enabled = reactive(TRUE))
  mod_hm2_server("hm2", enabled = reactive(TRUE))
  mod_hm3_server("hm3", enabled = reactive(TRUE))
  mod_hm4_server("hm4", enabled = reactive(TRUE))
  mod_hm5_server("hm5", enabled = reactive(TRUE))
  mod_hm6_server("hm6", enabled = reactive(TRUE))
  mod_hm7_server("hm7", enabled = reactive(TRUE))
  mod_hm8_server("hm8", enabled = reactive(TRUE))
  mod_hm9_server("hm9", enabled = reactive(TRUE))
  mod_hm10_server("hm10", enabled = reactive(TRUE))
  mod_hm14_server("hm14", enabled = reactive(TRUE))
  mod_hc35_server("hc35", enabled = reactive(TRUE))
  mod_fuel_server("fuel", enabled = reactive(TRUE))
  mod_bond_server("bond", enabled = reactive(TRUE))
}

server <- function(input, output) {
  tabs <- reactive(input$main_nav)

  observeEvent(input$main_nav, {
  cat("main_nav changed to:", input$main_nav, "\n")
}, ignoreInit = FALSE)

  register_modules(tabs)
  

  output$hb1_ui_lazy <- renderUI({
    print("here")
    req(input$main_nav == "hb1")
    mod_hb1_ui("hb1_main")
    
  })
  outputOptions(output, "hb1_ui_lazy", suspendWhenHidden = FALSE)

}
server2 <- function(input, output) {

  output$hs32_ui   <- renderUI({ui_hs32()})
  output$adp_ui    <- renderUI({ui_adp()})
  output$ect_ui    <- renderUI({ui_ect()})
  output$border_ui <- renderUI({ui_border()})
  output$slides_ui <- renderUI({ui_slides()})
  output$overview_ui      <- renderUI({ui_overview})


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

    data_grouped <- load_data("adpByRTO") %>%
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
}
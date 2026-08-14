

#update_border_all()
#load_data("border_all", refresh_cache = TRUE)
#load_series("border_all", max_unique = 300, refresh_cache = TRUE)

mod_border_ui <- function(id) {
  # ID Setup
  if (checks$ui_module) {print("border_ui loaded")}
  ns <- NS(id)
  # Get Metrics
  series <- load_series("border_all", max_unique = 300)
  nz_port <- series$`New Zealand Port`
  os_port <- series$`Overseas Port`
  pax_type <- series$`Passenger Type`
  travel_purpose <- series$`Travel Purpose`
  residency_country <- series$`Residency/Country`

  splits <- list(
    "New Zealand Port" = nz_port,
    "Overseas Port" = os_port,
    "Passenger Type" = pax_type,
    "Travel Purpose" = travel_purpose,
    "Residency/Country" = residency_country
  )

  # Get Inputs
  insert_inputs <- tagList(
    selectInput(ns("split_by"), "Split", choices = names(splits), selected = NULL, multiple = FALSE),
    selectInput(ns("nz_port"), "New Zealand Port", choices = nz_port, selected = NULL, multiple = TRUE),
    selectInput(ns("os_port"), "Overseas Port", choices = os_port, selected = NULL, multiple = TRUE),
    selectInput(ns("pax_type"), "Passenger Type", choices = pax_type, selected = NULL, multiple = TRUE),
    selectInput(ns("travel_purpose"), "Travel Purpose", choices = travel_purpose, selected = NULL, multiple = TRUE),
    selectInput(ns("residency_destination"), "Residency/Country", choices = residency_country, selected = NULL, multiple = TRUE),
    selectInput(ns("graph_type"), "Graph Type", choices = c("Area", "Bar", "Line", "Stacked Line" ), selected = "Area", multiple = FALSE),
    tags$div(
      class = "dl-compact dl-row",
      tags$div(
        class = "dl-wrap",
        actionLink(ns("dl_toggle"), label = "Download Settings", icon = icon("camera"), class = "dl-gear"),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] %% 2 == 1", ns("dl_toggle")),
          tags$div(class = "dl-panel", mod_download_ui(ns("dl")))
        )
      )
    )
  )
  ui_single(insert_inputs, p = ns("plot"), h = "600px")
}

mod_border_server <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    # Load data only when border tab is active
    border_data <- eventReactive(enabled(), {
      req(enabled())
      print("border_server loaded")
      load_data("border_all")
    }, ignoreInit = FALSE)

    dl <- mod_download_server("dl")

    border_plot <- reactive({
      req(enabled())
      d <- border_data()
      if (input$split_by == "Residency/Country" || input$split_by == "Travel Purpose") {
        d <- d %>% filter(Version %in% c("Destination", "Current"))
      } else {
        d <- d %>% filter(Version != "Destination")
      }

      if (!is.null(input$nz_port)) d <- d %>% filter(`New Zealand Port` %in% input$nz_port) else d <- d
      if (!is.null(input$os_port)) d <- d %>% filter(`Overseas Port` %in% input$os_port) else d <- d
      if (!is.null(input$pax_type)) d <- d %>% filter(`Passenger Type` %in% input$pax_type) else d <- d 
      if (!is.null(input$travel_purpose)) d <- d %>% filter(`Travel Purpose` %in% input$travel_purpose) else d <- d
      if (!is.null(input$residency_destination)) d <- d %>% filter(`Residency/Country` %in% input$residency_destination) else d <- d

      split <- if (!is.null(input$split_by)) input$split_by else NULL
      if (!is.null(split)) {
        d <- d %>%
          group_by(Date, across(all_of(split)))
      } else {
        d <- d %>%
          group_by(Date)
      }
      d <- d %>%
        summarise(Arrivals = sum(Arrivals), .groups = "drop")

      t <- if (checks$sourcenames) "Border Data" else "Border Data"

      valid_inputs <- unique(c(input$nz_port, input$os_port, input$pax_type, input$travel_purpose, input$residency_destination)) |> setdiff("-")
      graph_type <- input$graph_type
      s <- filter_series(
        guide_rbnz,
        apply_filters = list(Data = "border")
      )
      if (graph_type == "Bar") {
        s$Style <- "Bar"
      } else if (graph_type == "Area") {
        s$Style <- "Area"
      } else {
        s$Style <- "Line"
      }

      years_for_this <- switch(input$split_by,
        "New Zealand Port" = as.Date("2019-01-01"),
        "Overseas Port" = as.Date("2000-01-01"),
        "Passenger Type" = as.Date("2000-01-01"),
        "Travel Purpose" = as.Date("2000-01-01"),
        "Residency/Country" = as.Date("2000-01-01"),
        as.Date("2019-01-01")
      )
      years_for_this <- (Sys.Date() - years_for_this) / 365 %>% ceiling()
      years_for_this <- as.integer(years_for_this)

      x_plotly(
        data = d,
        titles = c(t, paste("StatsNZ:", short_title(valid_inputs))),
        series = s,
        k = "Date",
        years = years_for_this,
        download   = dl$download(),
        clean_ui   = dl$clean_export(),
        split      = split
      )
    })

    output$plot <- renderPlotly(border_plot())
  })
}


# ==============================================================================
# Bond UI ----------------------------------------------------------------------



mod_bond_ui <- function(id) {

  ns <- NS(id)
  ### Series selection ----
  bond_locations <- load_data("bond_locations") %>% unique()
  bond_locations <- bond_locations[bond_locations != "New Zealand"] %>% unique() %>% unlist() %>% unname()
  bond_tier <- filter_series(
    guide,
    column = c("Category_1", "ColumnName"),
    apply_filters = list(
      Dataset = "bond"
    )
  ) %>%
    dplyr::group_by(Category_1) %>%
    dplyr::summarise(
      value = list(ColumnName),
      .groups = "drop"
    ) %>%
    tibble::deframe() %>%
    lapply(\(x) stats::setNames(x, x))

  split_metric <- filter_series(
    guide,
    column = "ColumnName",
    apply_filters = list(Dataset = "bond")
  )
  split_metric <- c("-", split_metric)

  ### Create inputs ----
  insert_inputs <- tagList(
    selectInput(
      ns("bond_tier"),
      "Tier",
      choices = bond_tier,
      selected = unname(bond_tier[[1]][1]),
      multiple = TRUE
    ),
    selectInput(
      ns("bond_location"),
      "Location",
      choices = bond_locations,
      selected = bond_locations[1],
      multiple = TRUE
    ),

    selectInput(
      ns("bond_split_metric"),
      "Split by location",
      choices = split_metric,
      selected = "-",
      multiple = FALSE
    ),

    tags$div(
      class = "dl-compact dl-row",
      download_settings_ui(ns)
    )
  )

  ui_single(
    insert_inputs,
    p = ns("plot"),
    h = "600px",
    module = "bond"
  )
}


# ==============================================================================
# Bond Server ------------------------------------------------------------------

mod_bond_server <- function(id, selected_tab, activate_on) {

  moduleServer(id, function(input, output, session) {

    enabled <- reactive(
      identical(selected_tab(), activate_on)
    )


    # --------------------------------------------------------------------------
    # Selected series

    bond_series <- reactive({

      req(enabled())
      req(input$bond_tier)

      filter_series(
        guide,
        apply_filters = list(
          Dataset = "bond",
          ColumnName = input$bond_tier
        )
      )
    })


    # --------------------------------------------------------------------------
    # Plot

    output$plot <- renderPlotly({

      req(enabled())
      req(input$bond_tier)
      req(input$bond_location)

      chosen_locations <- if (length(input$bond_location) == 0) "New Zealand" else input$bond_location %>% unique() %>%
        unlist() %>%
        unname()
      print(chosen_locations)
      print("X")

      d <- load_data("bond") %>%
        dplyr::filter(
          Location %in% chosen_locations
        )

      s <- bond_series()
      a <- if (!all(chosen_locations == c("New Zealand")) && length(chosen_locations) > 0) "Location" else NULL
      print(a)

      standard_plot(
        data = d,
        titles = c(
          "Bond",
          paste("RBNZ:", short_title(input$bond_tier))
        ),
        series = s,
        split_by = if (!all(chosen_locations == c("New Zealand"))) "Location" else NULL,
        split_columns = if (input$bond_split_metric != "-") input$bond_split_metric else NULL,
        k = "Date"
      )
    })

  })
}
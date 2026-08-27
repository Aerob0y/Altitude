# ==============================================================================
# Bond UI ----------------------------------------------------------------------

mod_bond_ui_test <- function(id) {

  ns <- NS(id)

  bond_tier <- filter_series(
    guide,
    column = c("Category_1", "ColumnName"),
    apply_filters = list(
      Dataset = "bond"
    )
  ) |>
    dplyr::group_by(Category_1) |>
    dplyr::summarise(
      value = list(ColumnName),
      .groups = "drop"
    ) |>
    tibble::deframe() |>
    lapply(\(x) stats::setNames(x, x))

  split_metric <- filter_series(
  guide,
  column = "ColumnName",
  apply_filters = list(Dataset = "bond")
) |>
  unique() |>
  unlist() |>
  unname()

split_metric <- c("-", split_metric)

  # Locations
  bond_locations <- load_data("bond_locations") |>
    unique()

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
    module = "bond_test"
  )
}


# ==============================================================================
# Bond Server ------------------------------------------------------------------

mod_bond_server_test <- function(id, selected_tab, activate_on) {

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

      d <- load_data("bond") |>
        dplyr::filter(
          Location %in% input$bond_location
        )

      s <- bond_series()

standard_plot(
  data = d,
  titles = "X Plotly Test",
  series = s,
  split_by = if (input$bond_split_metric != "-") "Location" else NULL,
  split_columns = if (input$bond_split_metric != "-") input$bond_split_metric else NULL,
  k = "Date"
)
    })

  })
}

filter_series(guide, column = "ColumnName", apply_filters = list(Dataset = "bond")) |> unique() |> unlist() |> unname()

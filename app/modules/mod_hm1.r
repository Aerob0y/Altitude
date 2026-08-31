
mod_hm1_ui_update <- function(id) {
  add_log("mod_hm1_ui_update", "ui function called")
  # 1. Namespace
  ns <- NS(id)

  # 2. Series selection ----
  hm1_input <- filter_series_unlist(
    guide,
    column = "Category_2",
    apply_filters = list(Dataset = c("hm1"))
  )

  hm1_metric <- filter_series_unlist(
    guide,
    column = "Dim_Group",
    apply_filters = list(Dataset = c("hm1"))
  )

  # Defensive cleaning
  hm1_input  <- hm1_input[!is.na(hm1_input)]
  hm1_metric <- hm1_metric[!is.na(hm1_metric)]

  default_idx <- c(1, 4, 5, 6)
  default_idx <- default_idx[default_idx <= length(hm1_input)]
  default_inputs <- hm1_input[default_idx]

  # 3. Create inputs ----
  insert_inputs <- tagList(
    selectInput(
      ns("hm1_metric"),
      "Metric",
      choices = hm1_metric,
      selected = if ("y/y%" %in% hm1_metric) "y/y%" else hm1_metric[1],
      multiple = FALSE
    ),
    checkboxGroupInput(
      ns("hm1_input"),
      "Price Index (5 Max)",
      choices = hm1_input,
      selected = default_inputs
    )
  )

  # 4. Create UI ----
  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm1")
}


mod_hm1_server_update <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {

    # 1. Check if module is active
    enabled <- reactive(identical(selected_tab(), activate_on))

    # 2. Load data only when hm1 tab is active
    output$plot <- renderPlotly({
      req(enabled())
      req(input$hm1_input)
      valid_inputs <- input$hm1_input |> setdiff("-")
      valid_metric <- input$hm1_metric
      t <- if (checks$sourcenames) "Inflation - HM1" else "Inflation"

      standard_plot(
        data =          load_data("hm1"),
        titles =        c(t, paste("RBNZ:", valid_metric)),
        series =        filter_series(guide, apply_filters = list(Dataset = "hm1", Category_2 = valid_inputs, Dim_Group = valid_metric)),
        split_by =      NULL,
        split_columns = NULL,
        k = "Date"
      )
    })
  })
}

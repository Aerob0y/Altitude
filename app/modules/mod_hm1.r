mod_hm1_ui <- function(id) {
  if (checks$ui_module) print("hm1_ui loaded")
  ns <- NS(id)

  # Get Inputs (force to character vector)
  hm1_input <- filter_series(
    guide_rbnz,
    column = "Class_2",
    apply_filters = list(Data = c("hm1"))
  )$Class_2

  hm1_metric <- filter_series(
    guide_rbnz,
    column = "Dim",
    apply_filters = list(Data = c("hm1"))
  )$Dim

  # Defensive cleaning
  hm1_input  <- hm1_input[!is.na(hm1_input)]
  hm1_metric <- hm1_metric[!is.na(hm1_metric)]

  if (checks$ui_module) {
    print("hm1_input:")
    print(hm1_input)
    print("hm1_metric:")
    print(hm1_metric)
  }

  default_idx <- c(1, 4, 5, 6)
  default_idx <- default_idx[default_idx <= length(hm1_input)]
  default_inputs <- hm1_input[default_idx]

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

  ui_single(insert_inputs, p = ns("plot"), h = "600px", module = "hm1")
}


mod_hm1_server <- function(id, selected_tab, activate_on, plot_function = x_plotly) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    # Load data only when hm1 tab is active
    hm1_data <- eventReactive(enabled(), {
      req(enabled())
      print("hm1_server loaded")
      load_data("hm1")
    }, ignoreInit = FALSE)

    observeEvent(input$hm1_input, {
      req(input$hm1_input)
      if (length(input$hm1_input) > 5) {
        updateCheckboxGroupInput(
          session,
          "hm1_input",
          selected = input$hm1_input[1:5]
        )
      }
    }, ignoreInit = TRUE)

    hm1_plot <- reactive({
      req(enabled())
      d <- hm1_data()

      # Cap series count at 5, and handle empty selection
      valid_split <- input$hm1_input
      if (is.null(valid_split) || length(valid_split) == 0) {
        valid_split <- character(0)
      } else {
        valid_split <- valid_split[seq_len(min(length(valid_split), 5))]
      }

      valid_metric <- input$hm1_metric
      if (is.null(valid_metric) || length(valid_metric) == 0) {
        # fallback if nothing selected
        valid_metric <- if (length(filter_series(guide_rbnz, column = "Dim", apply_filters = list(Data = c("hm1")))) > 0) {
          "y/y%"
        } else {
          NULL
        }
      }

      t <- if (checks$sourcenames) "Prices - HM1" else "Prices"

      print("Testing hm1_plot with:")
      print(valid_split)
      print(valid_metric)
      plot_function(
        data = d,
        titles = c(t, paste("RBNZ:", valid_metric)),
        series = filter_series(
          guide_rbnz,
          apply_filters = list(
            Data = c("hm1"),
            Class_2 = valid_split,
            Dim = valid_metric
          )
        ),
        k = "Date"
      )
    })

    output$plot <- renderPlotly(hm1_plot())
  })
}

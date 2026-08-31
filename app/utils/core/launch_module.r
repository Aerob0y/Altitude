launch_module <- function(module, suffix = "") {

  ui_name <- paste0("mod_", module, "_ui", suffix)
  server_name <- paste0("mod_", module, "_server", suffix)

  if (!exists(ui_name)) {
    stop("UI function not found: ", ui_name)
  }

  if (!exists(server_name)) {
    stop("Server function not found: ", server_name)
  }

  ui_fun <- get(ui_name)
  server_fun <- get(server_name)

  ui <- fluidPage(
    ui_fun(module)
  )

  server <- function(input, output, session) {

    selected_tab <- reactive(module)

    server_fun(
      id = module,
      selected_tab = selected_tab,
      activate_on = module
    )
  }

  shinyApp(ui, server)
}

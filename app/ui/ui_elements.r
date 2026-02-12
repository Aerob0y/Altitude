ui_single <- function(insert_inputs, h = "600px", p) {
  if (checks$ui_elements) {print("Using ui_single")}  # DEBUG
  page_sidebar(
    sidebar = sidebar(
      class = "csv-sidebar",
      position = "left",
      #open = "closed",
      insert_inputs,
      style = "height: 100%;"
    ),
    card(
      #class = "csv-card",
      full_screen = TRUE,
      plotlyOutput(p,  height = h, width = "100%"),
      width = "100%",
      height = "100%",
      fill = TRUE
    )
  )
}
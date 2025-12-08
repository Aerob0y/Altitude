
ui_single <- function(insert_inputs, h = "600px", p) {
  div(
    style = "
      flex: 1 1 auto;
      display: flex;
      flex-direction: column;
      background-color: #EDF2F3 !important;
    ",

    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar",
        position = "left",
        insert_inputs,
        style = "height: 100%;"
      ),
      card(
        class = "csv-card",
        full_screen = TRUE,
        plotlyOutput(p,  height = h),
        style = "max-width: 1100px;"
      )
    )
  )
}

ui_single2 <- function(insert_inputs, h = "600px", p) {
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar",
        position = "left",
        open = "closed",
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



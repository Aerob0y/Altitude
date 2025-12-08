
ui_slides <- memoise(function() {
  ui_slides <- fluidPage(
    tags$head(tags$style(HTML("
      .slide-title { font-size: 26px; font-weight: 700; margin-bottom: 10px; }
      .slide-subtitle { font-size: 18px; font-weight: 600; margin-bottom: 20px; }
      .slide-text { font-size: 14px; }
    "))),
    # Navigation row
    fluidRow(
      column(
        3,
        actionButton("p", "◀ Previous")
      ),
      column(
        6,
        div(textOutput("slide_title"), class = "slide-title", align = "center")
      ),
      column(
        3,
        div(style = "text-align:right;",
          actionButton("n", "Next ▶")
        )
      )
    ),
    br(),
  # Body of the slide (plots + text) is generated dynamically
  uiOutput("slide_body")
)
})


ui_a <- bslib::page_fluid(
  bslib::accordion(
    bslib::accordion_panel(
      title = "Section A",
      icon = bsicons::bs_icon("menu-app"),
      "section A content"
    ),
    bslib::accordion_panel(
      title = "Section B",
      icon = bsicons::bs_icon("sliders"),
      bslib::layout_columns(
        card(
          mod_hb2_ui("hb2_a"),
          card("text")
        ),
        card(
          mod_hb2_ui("hb2_b"),
          card("text")
        ),
        col_widths = c(6, 6)
      )
    ),
    id   = "acc",
    open = "Section A"
  )
)
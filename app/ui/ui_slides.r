
ui_overview <- bslib::page_fluid(
  "Overview",
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

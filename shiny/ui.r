

source("shiny/ui/ui_rbnz.r")

themex <- bslib::bs_theme(
  base_font_weight = 500,
  "line-height-base" = 1.3
)

ui <- fluidPage(
  theme = themex,
  style = "background-color: #A6CAEC !important;",
  tags$head(
    tags$style(HTML("
      @media print {
      .rangeselector, .rangeselector-container { display: none !important;}
      }
      html, body, .page, .bslib-page, .bslib-card { height: 100%; }
      .bslib-page > .bslib-card { flex: 1 1 auto; }
      #hb1_plot { height: 100% !important; min-height: 0; }

      /* Sidebar background and text color */

      .csv-sidebar {
        background-color: #DCE6E8 !important;   /* light teal, change to whatever you want */
        color: #000000 !important;               /* text color inside sidebar */
      }
      .csv-card {
        color: #000000 !important;               /* text color inside sidebar */
      }
      
      
    "))
  ),
  navset_tab(
    nav_menu(
      "Economic Indicators",
      nav_panel("Daily exchange rates and TWI", ui_hb1x),
      nav_panel("Daily wholesale interest rates", ui_hb2x),
      nav_panel("Residential mortgage loan reconciliation", ui_hc35x),
      nav_panel("Prices", ui_hm1x),
      nav_panel("Consumption", ui_hm2x),
      nav_panel("Investment", ui_hm3x),
      nav_panel("Domestic Trade", ui_hm4x),
      nav_panel("GDP", ui_hm5x),
      nav_panel("National Saving", ui_hm6x),
      nav_panel("Balance of Payments", ui_hm7x),
      nav_panel("Overseas Trade", ui_hm8x),
      nav_panel("Labour Market", ui_hm9x),
      nav_panel("Housing", ui_hm10x),
      nav_panel("Expectations", ui_hm14x),
      nav_panel("Loans", ui_hs32x),
    )
  ),
  id = "tab",
  style = "background-color: #FFFFFF !important;"
)

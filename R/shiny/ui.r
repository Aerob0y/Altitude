





ui <- fluidPage(
  id = "root",   # give the outer page an ID we can hook CSS onto

  tags$head(
    tags$style(HTML("
      html, body, #root {
        height: 100%;
        margin: 0;
        padding: 0;
      }

      .bslib-gap-spacing {padding: 0px; margin: 0px; !important;}
      .plot-container {padding: 0px; margin: 0px; !important;}
      .tab-content,
      .tab-content > .tab-pane {
        padding: 0 !important;
      }

      /* The automatically-generated container-fluid inside fluidPage */
      #root > .container-fluid {
        height: 100%;
        padding: 0;
        margin: 0;
      }

      #tab .tab-pane {
      display: flex;
      flex-direction: column;
      flex: 1 1 auto;
      min-height: 0;
      padding: 0;
      margin: 0;
    }

      /* The tabset created by navset_tab */
      #root .tabbable {
        height: 100%;
        min-height: 0;
      }

      /* Content below the nav tabs */
      #root .tab-content {
        flex: 1 1 auto;

        min-height: 0;
      }

      /* Each tab pane */
      #root .tab-pane {
        flex: 1 1 auto;
        min-height: 0;
        padding: 0;
        margin: 0;
      }

      /* The divs like <div id=\"hb1_ui\" class=\"shiny-html-output shiny-bound-output\"> */
      #root .tab-pane > .shiny-html-output.shiny-bound-output {
        flex: 1 1 auto;
        min-height: 0;
        height: 800px;
        padding: 0;
        margin: 0;
      }

      /* Optional: your sidebar/card styling */
      .csv-sidebar {
        background-color: #DCE6E8 !important;
        color: #000000 !important;
        width: 500px;
      }
      .csv-card {
        color: #000000 !important;
      }
  /* Target the container that wraps the graph */
  .bslib-gap-spacing.html-fill-container {
    border: none !important;
    box-shadow: none !important;
    padding: 0 !important;
    margin: 0 !important;
  }


@media (max-width: 500px) {
  /* Target the container that wraps the graph */
  .bslib-gap-spacing.html-fill-container {
    border: none !important;
    box-shadow: none !important;
    padding: 0 !important;
    margin: 0 !important;
  }
}



    "))
  ),


  navset_tab(
  #navset_card(
    
    #nav_panel("Overview", uiOutput("a_ui")),
    nav_panel("Overview", ui_overview),
    nav_panel("Daily exchange rates and TWI", value = "hb1", uiOutput("hb1_ui_lazy")),
    nav_menu(
      "Economic Indicators",
      nav_panel("Daily wholesale interest rates", value = "hb2", mod_hb2_ui("hb2_main")),
      nav_panel("Residential mortgage loan reconciliation", value = "hc35", mod_hc35_ui("hc35")),
      nav_panel("Prices", value = "hm1", mod_hm1_ui("hm1")),
      nav_panel("Consumption", value = "hm2", mod_hm2_ui("hm2")),
      nav_panel("Investment", value = "hm3", mod_hm3_ui("hm3")),
      nav_panel("Domestic Trade", value = "hm4", mod_hm4_ui("hm4")),
      nav_panel("GDP", value = "hm5", mod_hm5_ui("hm5")),
      nav_panel("National Saving", value = "hm6", mod_hm6_ui("hm6")),
      nav_panel("Balance of Payments", value = "hm7", mod_hm7_ui("hm7")),
      nav_panel("Overseas Trade", value = "hm8", mod_hm8_ui("hm8")),
      nav_panel("Labour Market", value = "hm9", mod_hm9_ui("hm9")),
      nav_panel("Housing", value = "hm10", mod_hm10_ui("hm10")),
      nav_panel("Expectations", value = "hm14", mod_hm14_ui("hm14"))
      #nav_panel("ECT", uiOutput("ect_ui"))
    ),
    nav_menu(
      "Tourism Indicators",
      nav_panel("Fuel", value = "fuel", mod_fuel_ui("fuel")),
      #nav_panel("Accommodation", uiOutput("adp_ui")),
      #nav_panel("Border Movements", uiOutput("border_ui")),
      nav_panel("Bond Data", value = "bond", mod_bond_ui("bond"), id = "bond_data")
    ),
    id = "main_nav"
  )
)


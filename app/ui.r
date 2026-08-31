




ui <- fluidPage(
  id = "root",   # give the outer page an ID we can hook CSS onto
  tags$head(
    includeCSS("app/www/overview.css"),
    tags$style(HTML('

    #root .bslib-sidebar-layout {
  grid-template-columns: 350px 1fr !important;
}

    
.dl-toolbar {
  display: flex;
  align-items: center;
  gap: 6px;
  flex-wrap: nowrap;
}

/* Each Shiny input wrapper */
.dl-toolbar .form-group {
  margin: 0 !important;
  display: block;
}

/* Make inputs small */
.dl-toolbar .form-control {
  height: 26px !important;
  padding: 2px 6px !important;
  font-size: 11px !important;
}

/* Fixed widths so they dont collapse/overlap */
.dl-toolbar .dl-format  .form-control { width: 70px !important; }
.dl-toolbar .dl-width   .form-control { width: 80px !important; }
.dl-toolbar .dl-height  .form-control { width: 80px !important; }

/* Checkbox inline and compact */
.dl-toolbar .dl-clean {
  margin: 0 !important;
  white-space: nowrap;
  padding: 0 !important;
}
.dl-toolbar .dl-clean label {
  display: inline !important;
  font-size: 11px;
  margin: 0;
  padding: 0;
}
.dl-toolbar .dl-clean input[type="checkbox"] {
  margin: 0 4px 0 0 !important;
  position: relative;
  top: 1px;
}

 .dl-wrap { margin-bottom: 6px; }

.dl-gear {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  padding: 2px 6px;
  border: 1px solid rgba(0,0,0,0.15);
  border-radius: 6px;
  background: rgba(255,255,255,0.5);
  color: #111 !important;
  text-decoration: none !important;
  font-size: 12px;
}

.dl-gear:hover { background: rgba(255,255,255,0.8); }

.dl-panel {
  margin-top: 6px;
  padding: 6px;
  border: 1px solid rgba(0,0,0,0.12);
  border-radius: 8px;
  background: rgba(255,255,255,0.55);
}




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

  .shiny-input-select {
    background-color: rgb(255, 255, 255, 1) !important;
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



    '))
  ),


  navset_tab(
    nav_panel("Overview", value = "overview", ui_overview),
    nav_menu(
      "Economic indicators",
      nav_panel("Exchange Rates",  value = "hb1",  uiOutput("hb1_ui")),
      nav_panel("Interest Rates", value = "hb2", uiOutput("hb2_ui")),
      nav_panel("Residential Mortgages", value = "hc35", uiOutput("hc35_ui")),
      nav_panel("Inflation", value = "hm1", uiOutput("hm1_ui")),
      nav_panel("Consumption", value = "hm2", uiOutput("hm2_ui")),
      nav_panel("Investment", value = "hm3", uiOutput("hm3_ui")),
      nav_panel("Investment (reference test)", value = "hb3_test", uiOutput("hb3_test_ui")),
      nav_panel("Test", value = "hm14_test", uiOutput("hm14_test_ui")),
      nav_panel("Investment", value = "hm4", uiOutput("hm4_ui")),
      nav_panel("GDP", value = "hm5", uiOutput("hm5_ui")),
      nav_panel("National Savings", value = "hm6", uiOutput("hm6_ui")),
      nav_panel("Balance of Payments", value = "hm7", uiOutput("hm7_ui")),
      nav_panel("Overseas Trade", value = "hm8", uiOutput("hm8_ui")),
      nav_panel("Labour Market", value = "hm9", uiOutput("hm9_ui")),
      nav_panel("CoreLogic", value = "hm10", uiOutput("hm10_ui")),
      nav_panel("Survey of expectations", value = "hm14", uiOutput("hm14_ui")),
      nav_panel("Banks: Loans by product", value = "hs32", uiOutput("hs32_ui")),
      nav_panel("Canberra Catchment", value = "cbr_catchment",uiOutput("cbr_catchment_ui")),

      #nav_panel("ADP", value = "adp", uiOutput("adp_ui")),
      nav_panel("Bond", value = "bond", uiOutput("bond_ui")),
      nav_panel("BondTest", value = "bond_test", uiOutput("bond_test_ui")),
      nav_panel("BondTest2", value = "bond_test2", uiOutput("bond_test2_ui")),

    ),
    nav_menu(
      "Economic Indicators Update",
      nav_panel("Exchange Rates", value = "hb1_update", uiOutput("hb1_update_ui")),
      nav_panel("Interest Rates x", value = "hb2_update", uiOutput("hb2_update_ui")),
      nav_panel("Residential Mortgages", value = "hc35_update", uiOutput("hc35_update_ui")),
      nav_panel("Inflation", value = "hm1_update", uiOutput("hm1_update_ui")),
      nav_panel("Consumption", value = "hm2_update", uiOutput("hm2_update_ui")),
      nav_panel("Investment", value = "hm3_update", uiOutput("hm3_update_ui")),
      nav_panel("Domestic Trade", value = "hm4_update", uiOutput("hm4_update_ui")),
      nav_panel("GDP", value = "hm5_update", uiOutput("hm5_update_ui")),
      nav_panel("National Savings", value = "hm6_update", uiOutput("hm6_update_ui")),
      nav_panel("Balance of Payments", value = "hm7_update", uiOutput("hm7_update_ui")),
      nav_panel("Overseas Trade", value = "hm8_update", uiOutput("hm8_update_ui")),
      nav_panel("Labour Market", value = "hm9_update", uiOutput("hm9_update_ui")),
      nav_panel("CoreLogic", value = "hm10_update", uiOutput("hm10_update_ui")),
      nav_panel("Survey of expectations", value = "hm14_update", uiOutput("hm14_update_ui")),
      nav_panel("Banks: Loans by product", value = "hs32_update", uiOutput("hs32_update_ui"))
    ),
    nav_menu(
      "Tourism & Migration",
      nav_panel("Border", value = "border", uiOutput("border_ui")),
      nav_panel("Fuel", value = "fuel", uiOutput("fuel_ui")),
      nav_panel("NDM01", value = "ndm01", uiOutput("ndm01_ui"))
    ),

    id = "main_nav"
  )

)

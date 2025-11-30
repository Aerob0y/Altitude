

source("r/shiny/ui/ui_rbnz.r")
source("r/shiny/ui/ui_slides.r")


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
    nav_panel("Overview", uiOutput("slides_ui")),
    nav_menu(
      "Economic Indicators",
      nav_panel("Daily exchange rates and TWI", ui_hb1_main()),
      nav_panel("Daily wholesale interest rates", mod_hb2_ui("hb2_main")),
      nav_panel("Residential mortgage loan reconciliation", uiOutput("hc35_ui")),
      nav_panel("Prices", uiOutput("hm1_ui")),
      nav_panel("Consumption", uiOutput("hm2_ui")),
      nav_panel("Investment", uiOutput("hm3_ui")),
      nav_panel("Domestic Trade", uiOutput("hm4_ui")),
      nav_panel("GDP", uiOutput("hm5_ui")),
      nav_panel("National Saving", uiOutput("hm6_ui")),
      nav_panel("Balance of Payments", uiOutput("hm7_ui")),
      nav_panel("Overseas Trade", uiOutput("hm8_ui")),
      nav_panel("Labour Market", uiOutput("hm9_ui")),
      nav_panel("Housing", uiOutput("hm10_ui")),
      nav_panel("Expectations", uiOutput("hm14_ui")),
      nav_panel("ECT", uiOutput("ect_ui"))
    ),
    nav_menu(
      "Tourism Indicators",
      nav_panel("Fuel", uiOutput("fuel_ui")),
      nav_panel("Accommodation", uiOutput("adp_ui")),
      nav_panel("Border Movements", uiOutput("border_ui")),
      nav_panel("Bond Data", uiOutput("bond_ui"))
    ),
    id = "tab"
  )
)


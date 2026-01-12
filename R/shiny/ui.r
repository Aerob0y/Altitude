





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
    nav_panel("Overview", ui_overview),
    nav_panel("Daily exchange rates and TWI", value = "hb1", uiOutput("hb1_ui_lazy")),
    id = "main_nav"
  )
)



register_modules <- function(tabs) {
  mod_hb1_server("hb1_main", selected_tab = tabs)
}

server <- function(input, output, session) {

  tabs <- reactive(input$main_nav)
  register_modules(tabs)

  output$hb1_ui_lazy <- renderUI({
    req(input$main_nav == "hb1")
    mod_hb1_ui("hb1_main")
  })

}
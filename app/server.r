server <- function(input, output, session) {
  # keep track of active tab
  tab <- reactive(input$main_nav)

  #observeEvent(input$nav_hb1, {
  #  bslib::nav_select(
  #    id = "main_nav",
  #    selected = "hb1"
  #  )
  #})

  modules <- c(
    "hb2",
    "hb1",
    "hc35",
    "hm1",
    "hm2",
    "hm3",
    "hb3_test",
    "hm14_test",
    "hm4",
    "hm5",
    "hm6",
    "hm7",
    "hm8",
    "hm9",
    "hm10",
    "hm14",
    "hs32",
    "fuel_main",
    "bond_main",
    "bond_test_main",
    "border_main",
    "ndm01"
  )

  purrr::walk(
    modules,
    \(module) {
      observeEvent(
        input[[paste0("nav_", module)]],
        {
          print(paste("Navigating to module:", paste0("nav_", module)))
          bslib::nav_select(
            id = "main_nav",
            selected = module
          )
        }
      )
    }
  )

  # fire servers only when active
  mod_hb1_server("hb1_main",  selected_tab = tab, activate_on = "hb1")
  mod_hb1_server("hb1_main2", selected_tab = tab, activate_on = "hb1b")
  mod_hb2_server("hb2_main",  selected_tab = tab, activate_on = "hb2")
  mod_hc35_server("hc35_main",  selected_tab = tab, activate_on = "hc35")
  mod_hm1_server("hm1_main",  selected_tab = tab, activate_on = "hm1")
  mod_hm2_server("hm2_main",  selected_tab = tab, activate_on = "hm2")
  mod_hm3_server("hm3_main",  selected_tab = tab, activate_on = "hm3")
  mod_hb3_test_server("hb3_test_main", selected_tab = tab, activate_on = "hb3_test")
  #mod_hm14_test_server("hm14_test_main", selected_tab = tab, activate_on = "hm14_test")
  mod_hm4_server("hm4_main",  selected_tab = tab, activate_on = "hm4")
  mod_hm5_server("hm5_main",  selected_tab = tab, activate_on = "hm5")
  mod_hm6_server("hm6_main",  selected_tab = tab, activate_on = "hm6")
  mod_hm7_server("hm7_main",  selected_tab = tab, activate_on = "hm7")
  mod_hm8_server("hm8_main",  selected_tab = tab, activate_on = "hm8")
  mod_hm9_server("hm9_main",  selected_tab = tab, activate_on = "hm9")
  mod_hm10_server("hm10_main",  selected_tab = tab, activate_on = "hm10")
  mod_hm14_server("hm14_main",  selected_tab = tab, activate_on = "hm14")
  mod_hs32_server("hs32_main",  selected_tab = tab, activate_on = "hs32")
  mod_hb1_server("hb1_update_main", selected_tab = tab, activate_on = "hb1_update", plot_function = standard_plot)
  mod_hb2_server("hb2_update_main", selected_tab = tab, activate_on = "hb2_update", plot_function = standard_plot)
  mod_hc35_server("hc35_update_main", selected_tab = tab, activate_on = "hc35_update", plot_function = standard_plot)
  mod_hm1_server("hm1_update_main", selected_tab = tab, activate_on = "hm1_update", plot_function = standard_plot)
  mod_hm2_server("hm2_update_main", selected_tab = tab, activate_on = "hm2_update", plot_function = standard_plot)
  mod_hm3_server("hm3_update_main", selected_tab = tab, activate_on = "hm3_update", plot_function = standard_plot)
  mod_hm4_server("hm4_update_main", selected_tab = tab, activate_on = "hm4_update", plot_function = standard_plot)
  mod_hm5_server("hm5_update_main", selected_tab = tab, activate_on = "hm5_update", plot_function = standard_plot)
  mod_hm6_server("hm6_update_main", selected_tab = tab, activate_on = "hm6_update", plot_function = standard_plot)
  mod_hm7_server("hm7_update_main", selected_tab = tab, activate_on = "hm7_update", plot_function = standard_plot)
  mod_hm8_server("hm8_update_main", selected_tab = tab, activate_on = "hm8_update", plot_function = standard_plot)
  mod_hm9_server("hm9_update_main", selected_tab = tab, activate_on = "hm9_update", plot_function = standard_plot)
  mod_hm10_server("hm10_update_main", selected_tab = tab, activate_on = "hm10_update", plot_function = standard_plot)
  mod_hm14_server("hm14_update_main", selected_tab = tab, activate_on = "hm14_update", plot_function = standard_plot)
  mod_hs32_server("hs32_update_main", selected_tab = tab, activate_on = "hs32_update", plot_function = standard_plot)
  mod_fuel_server("fuel_main",  selected_tab = tab, activate_on = "fuel")
  #mod_adp_server("adp_main",  selected_tab = tab, activate_on = "adp")
  mod_bond_server("bond_main",  selected_tab = tab, activate_on = "bond")
  mod_bond_server_test("bond_test_main",  selected_tab = tab, activate_on = "bond_test")
  mod_border_server("border_main",  selected_tab = tab, activate_on = "border")
  mod_ndm01_server("ndm01",  selected_tab = tab, activate_on = "ndm01")

  # generate ui when needed
  output$hb1_ui <- renderUI({mod_hb1_ui("hb1_main")})
  output$hb1_ui2 <- renderUI({mod_hb1_ui("hb1_main2")})
  output$hb2_ui <- renderUI({mod_hb2_ui("hb2_main")})
  output$hc35_ui <- renderUI({mod_hc35_ui("hc35_main")})
  output$hm1_ui <- renderUI({mod_hm1_ui("hm1_main")})
  output$hm2_ui <- renderUI({mod_hm2_ui("hm2_main")})
  output$hm3_ui <- renderUI({mod_hm3_ui("hm3_main")})
  output$hb3_test_ui <- renderUI({mod_hb3_test_ui("hb3_test_main")})
  output$hm14_test_ui <- renderUI({mod_hm14_test_ui("hm14_test_main")})
  output$hm4_ui <- renderUI({mod_hm4_ui("hm4_main")})
  output$hm5_ui <- renderUI({mod_hm5_ui("hm5_main")})
  output$hm6_ui <- renderUI({mod_hm6_ui("hm6_main")})
  output$hm7_ui <- renderUI({mod_hm7_ui("hm7_main")})
  output$hm8_ui <- renderUI({mod_hm8_ui("hm8_main")})
  output$hm9_ui <- renderUI({mod_hm9_ui("hm9_main")})
  output$hm10_ui <- renderUI({mod_hm10_ui("hm10_main")})
  output$hm14_ui <- renderUI({mod_hm14_ui("hm14_main")})
  output$hs32_ui <- renderUI({mod_hs32_ui("hs32_main")})
  output$hb1_update_ui <- renderUI({mod_hb1_ui("hb1_update_main")})
  output$hb2_update_ui <- renderUI({mod_hb2_ui("hb2_update_main")})
  output$hc35_update_ui <- renderUI({mod_hc35_ui("hc35_update_main")})
  output$hm1_update_ui <- renderUI({mod_hm1_ui("hm1_update_main")})
  output$hm2_update_ui <- renderUI({mod_hm2_ui("hm2_update_main")})
  output$hm3_update_ui <- renderUI({mod_hm3_ui("hm3_update_main")})
  output$hm4_update_ui <- renderUI({mod_hm4_ui("hm4_update_main")})
  output$hm5_update_ui <- renderUI({mod_hm5_ui("hm5_update_main")})
  output$hm6_update_ui <- renderUI({mod_hm6_ui("hm6_update_main")})
  output$hm7_update_ui <- renderUI({mod_hm7_ui("hm7_update_main")})
  output$hm8_update_ui <- renderUI({mod_hm8_ui("hm8_update_main")})
  output$hm9_update_ui <- renderUI({mod_hm9_ui("hm9_update_main")})
  output$hm10_update_ui <- renderUI({mod_hm10_ui("hm10_update_main")})
  output$hm14_update_ui <- renderUI({mod_hm14_ui("hm14_update_main")})
  output$hs32_update_ui <- renderUI({mod_hs32_ui("hs32_update_main")})
  output$fuel_ui <- renderUI({mod_fuel_ui("fuel_main")})
  #output$adp_ui <- renderUI({mod_adp_ui("adp_main")})
  output$bond_ui <- renderUI({mod_bond_ui("bond_main")})
  output$bond_test_ui <- renderUI({mod_bond_ui_test("bond_test_main")})
  output$border_ui <- renderUI({mod_border_ui("border_main")})
  output$ndm01_ui <- renderUI({mod_ndm01_ui("ndm01")})



}

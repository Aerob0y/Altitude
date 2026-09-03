

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
    "hm8",
    "hm9",
    "hm10",
    "hm14",
    "hs32",
    "fuel_main",
    "bond",
    "bond",
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
  mod_hb1_server_update("hb1_update_main", selected_tab = tab, activate_on = "hb1_update")
  mod_hb2_server_update("hb2_update_main", selected_tab = tab, activate_on = "hb2_update")
  mod_hc35_server_update("hc35_update_main", selected_tab = tab, activate_on = "hc35_update")
  mod_hm1_server_update("hm1_update_main", selected_tab = tab, activate_on = "hm1_update")
  mod_hm2_server_update("hm2_update_main", selected_tab = tab, activate_on = "hm2_update")
  mod_hm3_server_update("hm3_update_main", selected_tab = tab, activate_on = "hm3_update")
  #mod_hm4_server_update("hm4_update_main", selected_tab = tab, activate_on = "hm4_update")
  #mod_hm5_server_update("hm5_update_main", selected_tab = tab, activate_on = "hm5_update")
  #mod_hm6_server_update("hm6_update_main", selected_tab = tab, activate_on = "hm6_update")
  #mod_hm7_server_update("hm7_update_main", selected_tab = tab, activate_on = "hm7_update")
  mod_hm8_server_update("hm8_update_main", selected_tab = tab, activate_on = "hm8_update")
  mod_hm9_server_update("hm9_update_main", selected_tab = tab, activate_on = "hm9_update")
  mod_hm10_server_update("hm10_update_main", selected_tab = tab, activate_on = "hm10_update")
  mod_hm14_server_update("hm14_update_main", selected_tab = tab, activate_on = "hm14_update")
  mod_hs32_server_update("hs32_update_main", selected_tab = tab, activate_on = "hs32_update")
  mod_fuel_server("fuel_main",  selected_tab = tab, activate_on = "fuel")
  #mod_adp_server("adp_main",  selected_tab = tab, activate_on = "adp")
  #mod_bond_server_test2("bond_test2_main",  selected_tab = tab, activate_on = "bond_test2")
  mod_bond_server("bond_main",  selected_tab = tab, activate_on = "bond")
  mod_ndm01_server("ndm01_main",  selected_tab = tab, activate_on = "ndm01")
  mod_cbr_catchment_server("cbr_catchment_main", selected_tab = tab, activate_on = "cbr_catchment")
  mod_nz_catchment_server("nz_catchment_main", selected_tab = tab, activate_on = "nz_catchment")

  # generate ui when needed
  output$hb3_test_ui <- renderUI({mod_hb3_test_ui("hb3_test_main")})
  output$hm14_test_ui <- renderUI({mod_hm14_test_ui("hm14_test_main")})
  output$hb1_update_ui <- renderUI({mod_hb1_ui_update("hb1_update_main")})
  output$hb2_update_ui <- renderUI({mod_hb2_ui_update("hb2_update_main")})
  output$hc35_update_ui <- renderUI({mod_hc35_ui_update("hc35_update_main")})
  output$hm1_update_ui <- renderUI({mod_hm1_ui_update("hm1_update_main")})
  output$hm2_update_ui <- renderUI({mod_hm2_ui_update("hm2_update_main")})
  output$hm3_update_ui <- renderUI({mod_hm3_ui_update("hm3_update_main")})
  output$hm4_update_ui <- renderUI({mod_hm4_ui_update("hm4_update_main")})
  output$hm5_update_ui <- renderUI({mod_hm5_ui_update("hm5_update_main")})
  output$hm6_update_ui <- renderUI({mod_hm6_ui_update("hm6_update_main")})
  output$hm7_update_ui <- renderUI({mod_hm7_ui_update("hm7_update_main")})
  output$hm8_update_ui <- renderUI({mod_hm8_ui_update("hm8_update_main")})
  output$hm9_update_ui <- renderUI({mod_hm9_ui_update("hm9_update_main")})
  output$hm10_update_ui <- renderUI({mod_hm10_ui_update("hm10_update_main")})
  output$hm14_update_ui <- renderUI({mod_hm14_ui_update("hm14_update_main")})
  output$hs32_update_ui <- renderUI({mod_hs32_ui_update("hs32_update_main")})
  output$cbr_catchment_ui <- renderUI({mod_cbr_catchment_ui("cbr_catchment_main")})
  output$nz_catchment_ui <- renderUI({mod_nz_catchment_ui("nz_catchment_main")})
  output$fuel_ui <- renderUI({mod_fuel_ui("fuel_main")})
  #output$adp_ui <- renderUI({mod_adp_ui("adp_main")})
  output$bond_ui <- renderUI({mod_bond_ui("bond_main")})
  output$ndm01_ui <- renderUI({mod_ndm01_ui("ndm01")})



}


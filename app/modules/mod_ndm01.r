mod_ndm01_ui <- function(id) {
  ns <- NS(id)
  class_1 <- c(
    "All house types",
    "Single house",
    "Scheme house",
    "Apartment"
  )
  ui_single(

    tagList(
      checkboxGroupInput(ns("type"), "Housing Type", choices = class_1, selected = class_1),
      tags$div(class = "dl-compact dl-row", download_settings_ui(ns))
    ),
    p = ns("plot"),
    h = "600px",
    module = "ndm01"
  )
}

#guide_rbnz$Data %>% unique() %>% sort()
#class_1
#x <- load_data("NDM01") %>%
#  rename("Date" = "TLIST(M1)") %>%
#  mutate(Date = as.Date(paste0(Date, "01"), format = "%Y%m%d"))

mod_ndm01_server <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    ndm01_data <- eventReactive(enabled(), {
      req(enabled())
      load_data("NDM01") %>%
        rename("Date" = "TLIST(M1)") %>%
        select(Date, `Dwelling Type`, VALUE) %>%
        mutate(Date = as.Date(paste0(Date, "01"), format = "%Y%m%d")) %>%
        mutate(VALUE = as.numeric(VALUE)) %>%
        pivot_wider(names_from = `Dwelling Type`, values_from = VALUE)
    }, ignoreInit = FALSE)

    dl <- mod_download_server("dl")

    ndm01_plot <- reactive({
      req(enabled())
      valid_inputs <- input$type |> unlist(use.names = FALSE) |> unique()
      print(valid_inputs)

      x_plotly(
        #data = filter(ndm01_data(), `Dwelling Type` %in% valid_inputs),
        data = ndm01_data(),
        titles = c("Daily exchange rates and TWI", paste("RBNZ:", short_title(valid_inputs))),
        #split = "Any",
        series = filter_series(
          guide_rbnz,
          apply_filters = list(Data = "NDM01", Name = "VALUE", Class_1 = valid_inputs)
        ),
        k = "Date",
        years = 15,
        download   = dl$download(),
        clean_ui   = dl$clean_export()
      )
    })


    output$plot <- renderPlotly(ndm01_plot())
  })
}


 

mod_ndm01_ui_update <- function(id) {
  # 1. Namespace
  ns <- NS(id)

  # 2. Series selection ----
  class_1 <- c(
    "All house types",
    "Single house",
    "Scheme house",
    "Apartment"
  )
  ui_single(

    tagList(
      checkboxGroupInput(ns("type"), "Housing Type", choices = class_1, selected = class_1),
      tags$div(class = "dl-compact dl-row", download_settings_ui(ns))
    ),
    p = ns("plot"),
    h = "600px",
    module = "ndm01"
  )
}

#guide_rbnz$Data %>% unique() %>% sort()
#class_1
#x <- load_data("NDM01") %>%
#  rename("Date" = "TLIST(M1)") %>%
#  mutate(Date = as.Date(paste0(Date, "01"), format = "%Y%m%d"))

mod_ndm01_server_update <- function(id, selected_tab, activate_on) {
  moduleServer(id, function(input, output, session) {
    enabled <- reactive(identical(selected_tab(), activate_on))

    ndm01_data <- eventReactive(enabled(), {
      req(enabled())
      load_data("NDM01") %>%
        rename("Date" = "TLIST(M1)") %>%
        select(Date, `Dwelling Type`, VALUE) %>%
        mutate(Date = as.Date(paste0(Date, "01"), format = "%Y%m%d")) %>%
        mutate(VALUE = as.numeric(VALUE)) %>%
        pivot_wider(names_from = `Dwelling Type`, values_from = VALUE)
    }, ignoreInit = FALSE)

    dl <- mod_download_server("dl")

    ndm01_plot <- reactive({
      req(enabled())
      valid_inputs <- input$type |> unlist(use.names = FALSE) |> unique()
      print(valid_inputs)

      x_plotly(
        #data = filter(ndm01_data(), `Dwelling Type` %in% valid_inputs),
        data = ndm01_data(),
        titles = c("Daily exchange rates and TWI", paste("RBNZ:", short_title(valid_inputs))),
        #split = "Any",
        series = filter_series(
          guide_rbnz,
          apply_filters = list(Data = "NDM01", Name = "VALUE", Class_1 = valid_inputs)
        ),
        k = "Date",
        years = 15,
        download   = dl$download(),
        clean_ui   = dl$clean_export()
      )
    })


    output$plot <- renderPlotly(ndm01_plot())
  })
}


 

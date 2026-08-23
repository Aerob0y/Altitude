module_notes <- list(
  hb1 = list("Exchange rates", "Reserve Bank of New Zealand (RBNZ)", "Shows how much foreign currency the New Zealand dollar buys. Use the selectors to compare currencies and the trade-weighted index."),
  hb2 = list("Interest rates", "Reserve Bank of New Zealand (RBNZ)", "Tracks wholesale and retail borrowing rates. Select up to five series to compare changes in borrowing conditions over time."),
  hc35 = list("Residential mortgages", "Reserve Bank of New Zealand (RBNZ)", "Shows mortgage lending across lending groups and loan types, providing context on household borrowing conditions."),
  hm1 = list("Inflation", "RBNZ and Stats NZ series", "Measures how prices change over time. Compare up to five headline or component price indexes using a common metric."),
  hm2 = list("Consumption", "RBNZ and Stats NZ series", "Describes household spending on goods and services—an important signal of demand and confidence in the economy."),
  hm3 = list("Investment", "RBNZ and Stats NZ series · HM3", "Covers spending that builds future capacity. This view compares broad investment types."),
  hb3_test = list("Investment — reference test", "RBNZ and Stats NZ series · HM3", "Uses the same HM3 data and selectors as Investment, rendered by the new reference-driven plotting workflow for comparison."),
  hm4 = list("Investment", "RBNZ and Stats NZ series · HM4", "A complementary investment view for exploring dimensions and domestic trade groups."),
  hm5 = list("GDP", "RBNZ and Stats NZ series", "Gross domestic product is the broadest measure of economic activity. Select up to four total, industry or per-person measures."),
  hm6 = list("National savings", "RBNZ and Stats NZ series", "Shows income retained rather than spent and the resources available to fund investment."),
  hm7 = list("Balance of payments", "RBNZ and Stats NZ series", "Records New Zealand’s economic transactions with the rest of the world, including trade, income and financing."),
  hm8 = list("Overseas trade", "RBNZ and Stats NZ series", "Tracks the value and volume of goods and services bought from and sold to other countries."),
  hm9 = list("Labour market", "RBNZ and Stats NZ series", "Brings together employment, participation, hours and wages to show how work and earnings are changing."),
  hm10 = list("CoreLogic", "CoreLogic series via RBNZ", "Provides housing-market measures such as property values. Movements vary by place and do not represent every home."),
  hm14 = list("Survey of expectations", "Reserve Bank of New Zealand (RBNZ)", "Summarises surveyed expectations for inflation and other economic measures. Expectations are opinions, not guaranteed forecasts."),
  hs32 = list("Banks: loans by product", "Reserve Bank of New Zealand (RBNZ)", "Breaks registered-bank lending into products and borrower groups to show where credit is growing or contracting."),
  bond = list("Bond", "New Zealand government bond data", "Shows yields and related measures for government debt. Yields reflect market expectations, term and risk, and can move quickly."),
  border = list("Border", "Stats NZ border movements", "Counts border crossings by port, passenger type, travel purpose and residency. Counts are movements, not unique people."),
  fuel = list("Fuel", "Public fuel-price series", "Tracks fuel prices over time, a practical indicator of transport costs for households, visitors and tourism operators.")
)

module_note <- function(module) {
  note <- module_notes[[module]]
  if (is.null(note)) return(NULL)

  tags$header(
    class = "module-header",
    tags$div(
      class = "module-header-title",
      tags$p(class = "module-kicker", "DATASET NOTES"),
      actionLink(
        inputId = paste0("nav_", module),
        label = tags$h1(note[[1]])
      )
    ),
    tags$div(
      class = "module-note",
      tags$p(class = "module-source", note[[2]]),
      tags$p(note[[3]])
    )
  )
}

ui_single <- function(insert_inputs, h = "600px", p, module = NULL) {
  if (checks$ui_elements) print("Using ui_single")

  tags$div(
    class = "module-page",
    module_note(module),
    page_sidebar(
      sidebar = sidebar(
        class = "csv-sidebar",
        position = "left",
        insert_inputs,
        style = "height: 100%;"
      ),
      card(
        class = "module-chart-card",
        full_screen = TRUE,
        plotlyOutput(p, height = h, width = "100%"),
        width = "100%",
        height = "100%",
        fill = TRUE
      )
    )
  )
}

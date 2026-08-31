overview_dataset_card <- function(module, icon, title, source, description) {

  actionLink(
    inputId = paste0("nav_", module),
    class = "overview-dataset-link",

    tags$article(
      class = "overview-dataset-card",

      tags$div(
        class = "overview-dataset-icon",
        bsicons::bs_icon(icon)
      ),

      tags$div(
        tags$h3(title),
        tags$p(class = "overview-dataset-source", source),
        tags$p(description)
      )
    )
  )
}



ui_overview <- tags$div(
  class = "overview-page",
  tags$section(
    class = "overview-hero",
    tags$div(
      class = "overview-hero-content",
      tags$p(class = "overview-eyebrow", "NEW ZEALAND ECONOMIC & TOURISM DATA"),
      tags$h1("A clearer view of what is shaping Aotearoa"),
      tags$p(
        class = "overview-intro",
        "Altitude brings trusted public datasets together in one place, turning detailed economic, travel and migration data into charts that are easier to explore."
      ),
      tags$div(
        class = "overview-hero-actions",
        tags$a(class = "overview-button", href = "#dataset-guide", "Explore the datasets"),
        tags$span(class = "overview-scroll-note", bsicons::bs_icon("arrow-down"), " Start with the guide below")
      )
    ),
    tags$a(
      class = "overview-photo-credit",
      href = "https://images.squarespace-cdn.com/content/v1/68b1048a124d6a2d1847e242/8627946e-09b3-4514-b78b-5fd81f50907f/734029-milford-sound-scenic-flight-true-south-flights-web-1920px.jpg",
      target = "_blank",
      rel = "noopener noreferrer",
      "Milford Sound · True South Flights"
    )
  ),
  tags$main(
    class = "overview-content",
    tags$section(
      class = "overview-welcome",
      tags$div(
        tags$p(class = "overview-kicker", "ABOUT THIS DASHBOARD"),
        tags$h2("The big picture, without the jargon"),
        tags$p("Use the menus above to move between topics. Each page lets you choose a series, adjust the date range and inspect values on the chart. The download controls can save a chart or its underlying data for your own work.")
      ),
      tags$aside(
        class = "overview-callout",
        bsicons::bs_icon("info-circle"),
        tags$div(
          tags$strong("How to read the charts"),
          tags$p("Check the unit, frequency and date range before comparing series. Recent observations may be revised by the original data provider.")
        )
      )
    ),
    tags$section(
      id = "dataset-guide",
      class = "overview-guide",
      tags$p(class = "overview-kicker", "DATASET GUIDE"),
      tags$h2("What each dataset can tell you"),
      tags$p(class = "overview-section-intro", "Plain-language notes to help you choose the right chart. Dataset names match the dashboard menus."),
      tags$h3(class = "overview-group-title", "Economic indicators"),
      tags$div(
        class = "overview-dataset-grid",
        overview_dataset_card("x", "currency-exchange", "Exchange rates", "Reserve Bank of New Zealand (RBNZ)", "Shows how much foreign currency the New Zealand dollar buys. Useful for understanding the cost of imports, overseas travel and export competitiveness."),
        overview_dataset_card("hb2", "percent", "Interest rates", "RBNZ", "Tracks wholesale and retail borrowing rates. These rates influence mortgage payments, business finance and returns to savers."),
        overview_dataset_card("hm1", "house", "Residential mortgages", "RBNZ", "Shows mortgage lending and advertised rates across different loan terms, giving context on household borrowing conditions."),
        overview_dataset_card("hm2", "graph-up-arrow", "Inflation", "RBNZ and Stats NZ series", "Measures how prices change over time. Headline and component series help show where cost-of-living pressure is coming from."),
        overview_dataset_card("hm3", "cart3", "Consumption", "RBNZ and Stats NZ series", "Describes household spending on goods and services—an important signal of demand and confidence in the economy."),
        overview_dataset_card("hm4", "building", "Investment", "RBNZ and Stats NZ series", "Covers spending that builds future capacity, including construction, machinery and other capital. Two dashboard views provide complementary investment series."),
        overview_dataset_card("hm5", "bar-chart-line", "GDP", "RBNZ and Stats NZ series", "Gross domestic product is the broadest measure of economic activity. Compare total, industry and per-person measures with care."),
        overview_dataset_card("hm6", "piggy-bank", "National savings", "RBNZ and Stats NZ series", "Shows the share of income retained rather than spent by households, businesses and government, and the resources available to fund investment."),
        overview_dataset_card("hm7", "arrow-left-right", "Balance of payments", "RBNZ and Stats NZ series", "Records New Zealand’s economic transactions with the rest of the world, including trade, investment income and international financing."),
        overview_dataset_card("hm8", "box-seam", "Overseas trade", "RBNZ and Stats NZ series", "Tracks the value and volume of goods and services bought from and sold to other countries."),
        overview_dataset_card("hm9", "people", "Labour market", "RBNZ and Stats NZ series", "Brings together employment, unemployment, participation, hours and wages to show how work and earnings are changing."),
        overview_dataset_card("hm10", "houses", "CoreLogic", "CoreLogic series via RBNZ", "Provides housing-market measures such as property values. Movements vary by place and do not represent the value of every home."),
        overview_dataset_card("hm14", "chat-square-text", "Survey of expectations", "RBNZ", "Summarises what surveyed businesses and forecasters expect for inflation and other economic measures. Expectations are opinions, not forecasts guaranteed to occur."),
        overview_dataset_card("hb1", "bank", "Banks: loans by product", "RBNZ", "Breaks registered-bank lending into products and borrower groups, helping show where credit is growing or contracting."),
        overview_dataset_card("hb1", "graph-up", "Bond", "New Zealand government bond data", "Shows yields and related measures for government debt. Bond yields reflect market expectations, term and risk, and can move quickly.")
      ),
      tags$h3(class = "overview-group-title", "Tourism & migration"),
      tags$div(
        class = "overview-dataset-grid",
        overview_dataset_card("hb1", "airplane", "Border", "Stats NZ border movements", "Counts people crossing New Zealand’s border and helps distinguish visitor arrivals, resident travel and migration patterns. Counts are movements, not unique people."),
        overview_dataset_card("hb1", "fuel-pump", "Fuel", "Public fuel-price series", "Tracks fuel prices over time, a practical indicator of transport costs for households, visitors and tourism operators.")
      )
    ),
    tags$section(
      class = "overview-data-note",
      tags$div(bsicons::bs_icon("clipboard-data")),
      tags$div(
        tags$h2("A note about the data"),
        tags$p("Altitude presents information supplied by external organisations. Definitions, seasonal adjustment, release schedules and revision practices differ between series. Use the source name and chart labels as your starting point, and consult the original provider before making important decisions.")
      )
    )
  )
)

register_function("app/ui/ui_slides.r", "overview_dataset_card", "Creates a card for the overview page dataset guide")
register_function("app/ui/ui_slides.r", "ui_overview", "Generates the overview page UI with hero section, dataset guide, and data note")
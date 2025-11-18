fuel_plotly <- function() {
  generic_plotly(
      data = .load_data('fuel'),
      series = .cache$series %>% filter(Graph == "fuel"),
      k = "Date",
      t1 = "Jet Fuel Monthly Price",
      t2 = "US Dollars per Barrel",
      dim = c("$"),
      fallback = NULL
    )
}
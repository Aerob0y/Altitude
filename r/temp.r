

  data <- load_data("bond")
  data <- data %>% filter(Location %in% c("ALL"))
  head(data)
data$`Lodged Bonds` %>% class()

  generic_plotly(
      data = data,
      titles = c("Bond Prices", paste("In", "")),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("bond"), Names = c("Lodged Bonds"))),
      k = "Date",
      split = "Location"
    )

    plot_long(
      data = data,
      titles = c("Bond Prices", ""),
      series = filter_series(guide_rbnz, apply_filters = list(Graph = c("bond"), Names = c("Closed Bonds"))),
      k = "Date",
      split = "Location"
    )

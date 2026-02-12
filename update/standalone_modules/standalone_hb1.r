
# 1) Load data and guide
d <- load_data("hb1")

# 2) Choose a fixed set of series (example)
valid_inputs <- c("NZD/USD")  # <- set whatever you want exported

series <- filter_series(
  guide_rbnz,
  apply_filters = list(Data = "hb1", Class_1 = valid_inputs)
)

series <- filter_series(
  guide_rbnz,
  apply_filters = list(Data = "hb1")
)

# 3) Build plot
p <- x_plotly(
  data   = d,
  titles = c("Daily exchange rates and TWI", "RBNZ"),
  series = series,
  k      = "Date",
  years  = 15,
  # set your export defaults (these affect the camera button too if you keep them)
  download = list(format="png", width=1920, height=1080, scale=2),
  clean_ui = TRUE
)
p
# 4) Export to JSON spec
plotly_to_spec_json(p, "C:/Users/MichaelHawley/OneDrive - CSV Limited/Published/Standalone/hb1/hb1.json")

getwd()
series %>% view()

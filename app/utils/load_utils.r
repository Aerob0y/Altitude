# Load shared application utilities in dependency order.
#
# Keep this list explicit: utility files define objects that are consumed by
# files later in the list, so recursively sourcing the directory would make
# startup order depend on file names.
utility_files <- c(
  "app/utils/core/dependencies.r",
  "app/utils/plotting/standards.r",
  "app/utils/shiny/download.r",
  "app/utils/data/data.r",
  "app/utils/plotting/plotly.r",
  "app/utils/plotting/standard_plot.r",
  "app/utils/plotting/reference_plot.r",
  "app/utils/plotting/standard_module.r"
)


invisible(lapply(
  utility_files,
  source,
  local = FALSE,
  echo = FALSE
))

rm(utility_files)

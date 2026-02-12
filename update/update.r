


updates <- function(what = "", manual = FALSE) {
  if (manual || !interactive()) {
    # Load helper functions
    source("update/update_utilities/update_libraries.r") # data sources, caching, filtering
    source("update/update_utilities/update_etag.r") # data sources, caching, filtering
    source("update/update_utilities/export_plotly_spec.r") # data sources, caching, filtering

    if (what == "" || what == "data") {
      purrr::walk(
      list.files("app/r/utils", full.names = TRUE, pattern = "utils_data"),
      ~ source(.x, local = FALSE, echo = FALSE)
      )
      purrr::walk(
        list.files("update/update_modules", full.names = TRUE, pattern = "\\.r$"),
        ~ source(.x, local = FALSE, echo = FALSE)
      )
    }
  }
}
updates()



cache <- new.env(parent = emptyenv())
guide_rbnz <- readRDS("data/Reference/RBNZ_Series.rds")

filter_series <- function(guide, column = NULL, apply_filters = NULL, apply_fallbacks = NULL) {
  t <- guide
  if (!is.null(apply_filters) || !is.null(apply_fallbacks)) {
    for (f in seq_along(apply_filters)) {
      filter_name <- names(apply_filters)[f]
      filter_values <- apply_filters[[f]]
      if (length(filter_values) > 0) {
        t <- t |>
          filter(get(filter_name) %in% filter_values)
      }
    }
    if (nrow(t) == 0) {
      for (f in seq_along(apply_fallbacks)) {
        filter_name <- names(apply_fallbacks)[f]
        filter_values <- apply_fallbacks[[f]]
        if (length(filter_values) > 0) {
          t <- guide |>
            filter(get(filter_name) %in% filter_values)
        }
      }
    }
  }
  if (!is.null(column)) {
    t <- t |>
      select(all_of(column)) |>
      unique()
    t <- t[complete.cases(t), ]
  }
  if (checks$filter_series) {print(t)}
  t
}

load_data <- function(name, refresh_cache = FALSE) {
  if (checks$load_data) print(paste0("load_data: ", name))
  if (exists(name, envir = cache) && !refresh_cache) {return(cache[[name]])}
  f <- list.files(
    "data",
    pattern = paste0(name, ".rds"),
    recursive = TRUE,
    full.names = FALSE
  )
  if (length(f) == 0) {
    stop("No RDS file found for dataset '", name, "' in data/ directory")
  }
  f <- paste0("data/", f)
  cache[[name]] <- readRDS(f[1])
  cache[[name]]
}
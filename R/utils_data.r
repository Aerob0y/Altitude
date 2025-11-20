.cache <- new.env(parent = emptyenv())
#guide_rbnz <- openxlsx::read.xlsx("reference/RBNZ_Series.xlsx", detectDates = TRUE, sheet = "Series Definitions", startRow = 1, skipEmptyRows = TRUE)
guide_rbnz <- readRDS("reference/RBNZ_Series.rds")
#saveRDS(guide_rbnz, file = "reference/RBNZ_Series.rds", compress = FALSE)

filter_series <- function(guide, column = NULL, apply_filters = NULL, apply_fallbacks = NULL) {
  #if (!column %in% names(guide) && !is.null(column)) {stop(sprintf("Unknown column '%s'. Try one of: %s", column, paste(names(guide), collapse = ", ")))}

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
    t[complete.cases(t), ]
  } else {
    t
  }
}

load_data <- function(name) {
  if (exists(name, envir = .cache)) {return(.cache[[name]])}
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
  .cache[[name]] <- readRDS(f[1])
  .cache[[name]]
}
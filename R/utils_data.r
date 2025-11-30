.cache <- new.env(parent = emptyenv())
guide_rbnz <- readRDS("reference/RBNZ_Series.rds")

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
    t[complete.cases(t), ]
  } else {
    t
  }
}

load_data <- function(name, refresh_cache = FALSE) {
  if (exists(name, envir = .cache) && !refresh_cache) {return(.cache[[name]])}
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

load_fuel <- function() {
  if (exists("fuel_convert", envir = .cache)) {return(.cache[["fuel_convert"]])}
  nzd_usd <- load_data("hb1") %>% select(Date, EXR.DS11.D06) %>% rename(Date = Date, USD_NZD = EXR.DS11.D06)
  result <- merge(load_data("fuel"), nzd_usd, by = 'Date', all.x = TRUE)
  result <- arrange(result, Date)
  result <- fill(result, USD_NZD, .direction = "down")
  result <- rename(result, c("USD per Barrel" = "Price"))
  result <- filter(result, !is.na(USD_NZD))
  result <- result %>% mutate('NZD per Barrel' = (`USD per Barrel`/ USD_NZD))
  .cache[["fuel_convert"]] <- result
  .cache[["fuel_convert"]]
}


load_adp <- function() {
  if (exists("adpByRTO", envir = .cache)) {return(.cache[["adpByRTO"]])}
  if (file.exists("data/ADP/adpByRTO.rds")) {
    .cache[["adpByRTO"]] <- readRDS("data/ADP/adpByRTO.rds")
    .cache[["adpByRTO"]]
  }
  else {
    stop("No RDS file found for ADP dataset in data/ADP/ directory")
  }
}

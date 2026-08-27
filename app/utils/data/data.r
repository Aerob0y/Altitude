checks <- list(
  load_data = FALSE,
  filter_series = FALSE,
  ui_elements = FALSE,
  sourcenames = TRUE,
  ui_module = TRUE
)

load_cached_rds_old <- function(csv_file, rds_file) {

  if (!file.exists(csv_file)) {
    stop("CSV file does not exist: ", csv_file)
  }

  rebuild <- !file.exists(rds_file) ||
    file.info(csv_file)$mtime > file.info(rds_file)$mtime

  if (rebuild) {
    print(paste0("Rebuilding RDS file from CSV: ", rds_file))

    data <- readr::read_csv(
      csv_file,
      show_col_types = FALSE
    )

    saveRDS(data, rds_file)

  } else {
    print(paste0("Loading RDS file: ", rds_file))
    data <- readRDS(rds_file)

  }

  data
}

load_cached_rds <- function(file, rds_file, table = NULL, sheet = NULL) {

  if (!file.exists(file)) {
    stop("Source file does not exist: ", file)
  }

  rebuild <- !file.exists(rds_file) ||
    file.info(file)$mtime > file.info(rds_file)$mtime

  if (!rebuild) {
    message("Loading RDS file: ", rds_file)
    return(readRDS(rds_file))
  }

  message("Rebuilding RDS file from: ", file)

  ext <- tolower(tools::file_ext(file))

  data <- switch(
    ext,

    csv = readr::read_csv(
      file,
      show_col_types = FALSE
    ),

    xlsx = readxl::read_excel(
      file,
      sheet = sheet
    ),

    stop("Unsupported file type: ", ext)
  )

  saveRDS(data, rds_file)

  data
}


cache <- new.env(parent = emptyenv())
#guide_rbnz <- readRDS("data/Reference/RBNZ_Series.rds")


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

filter_series_unlist <- function(guide, column = NULL, apply_filters = NULL, apply_fallbacks = NULL) {
  t <- filter_series(guide, column, apply_filters, apply_fallbacks)
  t <- t |>
    as.vector() |>
    unlist() |>
    unname()
  t
}



load_data <- function(name, refresh_cache = FALSE) {
  if (checks$load_data) print(paste0("load_data: ", name))
  if (exists(name, envir = cache) && !refresh_cache) {return(cache[[name]])}
  f <- list.files(
    ".",
    pattern = paste0(name, ".rds"),
    recursive = TRUE,
    full.names = FALSE
  )
  if (length(f) == 0) {
    stop("No RDS file found for dataset '", name, "' in data/ directory")
  }
  cache[[name]] <- readRDS(f[1])
  cache[[name]]
}

guide <- load_cached_rds(
  "app/data/Reference/guide.csv",
  "app/data/Reference/guide.rds"
)

guide_rbnz <- load_cached_rds(
  "app/data/Reference/RBNZ_Series.csv",
  "app/data/Reference/RBNZ_Series.rds"
) %>% dplyr::mutate(
    Style = factor(
      Style,
      levels = c("Fill", "Bar", "Line"),
      ordered = TRUE
    )
  )

guide <- load_cached_rds(
  "app/data/Reference/guide.xlsx",
  "app/data/Reference/guide.rds",
  table = "Guide", sheet = "Guide"
)
guide %>% glimpse()

load_series <- function(data_name, drop_na = FALSE, max_unique = 100, refresh_cache = FALSE) {
  data_series <- paste(data_name, "series", sep = "_")
  if (checks$load_data) print(paste0("load_series: ", data_series))
  if (exists(data_series, envir = cache) && !refresh_cache) {return(cache[[data_series]])}
  df <- load_data(data_name)
  out <- lapply(df, function(x) {
    u <- unique(x)
    if (drop_na) u <- u[!is.na(u)]
    u
  })
  names(out) <- names(df)
  n <- out[sapply(out, length) <= max_unique]
  cache[[data_series]] <- n
  cache[[data_series]]
}

# cache: establish a cache environment to store loaded data and avoid redundant reads ----
cache <- new.env(parent = emptyenv())

# load_cached_rds: Load data from a CSV or Excel file and cache it as an RDS file for faster future access ----
load_cached_rds <- function(file, rds_file, table = NULL, sheet = NULL) {
  if (!file.exists(file)) {stop("Source file does not exist: ", file)}

  rebuild <- !file.exists(rds_file) || file.info(file)$mtime > file.info(rds_file)$mtime

  if (!rebuild) {
    message("Loading RDS file: ", rds_file)
    return(readRDS(rds_file))
  }

  message("Rebuilding RDS file from: ", file)
  ext <- tolower(tools::file_ext(file))

  data <- switch(ext,
    csv = readr::read_csv(file, show_col_types = FALSE),
    xlsx = readxl::read_excel(file, sheet = sheet),
    stop("Unsupported file type: ", ext)
  )

  saveRDS(data, rds_file)
  data
}

local({

  apply_series_filters <- function(data, filters) {

    for (f in seq_along(filters)) {

      filter_name <- names(filters)[f]
      filter_values <- filters[[f]]

      if (length(filter_values) > 0) {
        data <- data |>
          dplyr::filter(.data[[filter_name]] %in% filter_values)
      }
    }

    data
  }


  filter_series <<- function(
      guide,
      column = NULL,
      apply_filters = NULL,
      apply_fallbacks = NULL
  ) {

    t <- apply_series_filters(guide, apply_filters)

    if (nrow(t) == 0) {
      t <- apply_series_filters(guide, apply_fallbacks)
    }
    if (!is.null(column)) {
      t <- t |>
        select(all_of(column)) |>
        unique()
      t <- t[complete.cases(t), ]
    }
    if (length(column) == 1) {
      t <- t[[1]] |>
        as.vector() |>
        unlist() |>
        unname()
    }
    t
  }

})

## load_data: Load an RDS file into the cache ----
load_data <- function(name, refresh_cache = FALSE) {
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

#guide_rbnz - Load the RBNZ series guide and convert the Style column to an ordered factor ----
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

## guide: Load the common guide ----
guide <- readr::read_csv(
  "app/data/Reference/guide.csv",
  show_col_types = FALSE
)

# Keep the module-facing names available while the updated plots consume the
# richer, common guide schema. This lets a module receive either guide without
# maintaining two versions of its filtering logic.
guide <- guide |>
  dplyr::mutate(
    Data = Dataset,
    ID = DatasetColumn,
    Name = ColumnName,
    Class_1 = Category_1,
    Class_2 = Category_2,
    Class_3 = Level_1,
    Unit = Dim_Label,
    Dim = Dim_Group
  )

# load_series: Load unique series values for a given dataset, with options to drop NAs and limit the number of unique values ----
load_series <- function(data_name, drop_na = FALSE, max_unique = 100, refresh_cache = FALSE) {
  data_series <- paste(data_name, "series", sep = "_")
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

guide <- load_cached_rds(
  "app/data/Reference/Guide.xlsx",
  "app/data/Reference/Guide.rds",
  sheet = "Guide", table = "Guide"
)

register_function("app/utils/data/data.r", "load_cached_rds", "Loads data from a CSV or Excel file and caches it as an RDS file for faster future access")
register_function("app/utils/data/data.r", "filter_series", "Filters a guide data frame based on specified columns and fallback options")
register_function("app/utils/data/data.r", "load_data", "Loads an RDS file into the cache")
register_function("app/utils/data/data.r", "load_series", "Loads unique series values for a given dataset, with options to drop NAs and limit the number of unique values")

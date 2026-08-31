
#
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


filter_series_unlist <- function(guide, column = NULL, apply_filters = NULL, apply_fallbacks = NULL) {
  t <- filter_series(guide, column, apply_filters, apply_fallbacks)
  t <- t |>
    as.vector() |>
    unlist() |>
    unname()
  t
}
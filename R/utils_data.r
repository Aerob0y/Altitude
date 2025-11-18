date_summarise <- function(df, date_col) {
  df %>%
    mutate({{date_col}} := lubridate::floor_date({{date_col}}, "week", week_start = 1)) %>%
    group_by({{date_col}}) %>%
    summarise(
      across(
        .cols = everything(),        # all cols incl Date
        .fns = list(mean = ~ mean(.x, na.rm = TRUE)),
        .names = "{.col}"
      ),
      .groups = "drop"
    )
}

simple_pivot_wider <- function(df, names_from = "series", values_from = "value") {
  df %>%
    pivot_wider(
      names_from = {{names_from}},
      values_from = {{values_from}}
    )
}


latest_file <- function(dir = "data-raw/RBNZ", pattern = "^hb1-daily-\\d{8}\\.xlsx$") {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) stop("No matching files in ", dir)
  files[order(files, decreasing = TRUE)][1]
}

.cache <- new.env(parent = emptyenv())
.cache$data <- list()
.cache$series <- openxlsx::read.xlsx("reference/RBNZ_Series.xlsx", detectDates = TRUE, sheet = "Series Definitions", startRow = 1, skipEmptyRows = TRUE)
.cache$paths <- list(
  hm1  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data-raw/RBNZ", pattern = "^hm1-\\d{8}\\.xlsx$"))),
  hm2  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data-raw/RBNZ", pattern = "^hm2-\\d{8}\\.xlsx$"))),
  hm3  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data-raw/RBNZ", pattern = "^hm3-\\d{8}\\.xlsx$"))),
  hm4  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data-raw/RBNZ", pattern = "^hm4-\\d{8}\\.xlsx$"))),
  hm5  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data-raw/RBNZ", pattern = "^hm5-\\d{8}\\.xlsx$"))),
  hm6  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data-raw/RBNZ", pattern = "^hm6-\\d{8}\\.xlsx$"))),
  hm7  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data-raw/RBNZ", pattern = "^hm7-\\d{8}\\.xlsx$"))),
  hm8  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data-raw/RBNZ", pattern = "^hm8-\\d{8}\\.xlsx$"))),
  hm9  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data-raw/RBNZ", pattern = "^hm9-\\d{8}\\.xlsx$"))),
  hm10 = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data-raw/RBNZ", pattern = "^hm10-\\d{8}\\.xlsx$"))),
  hm14 = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data-raw/RBNZ", pattern = "^hm14-\\d{8}\\.xlsx$"))),
  hs32 = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data-raw/RBNZ", pattern = "^hs32-\\d{8}\\.xlsx$"))),
  hc35 = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data-raw/RBNZ", pattern = "^hc35-\\d{8}\\.xlsx$"))),
  hb1 = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(
    latest_file(dir = "data-raw/RBNZ", pattern = "^hb1-daily-\\d{8}\\.xlsx$"),
    latest_file(dir = "data-raw/RBNZ", pattern = "^hb1-daily-1999-2017-\\d{8}\\.xlsx$"),
    latest_file(dir = "data-raw/RBNZ", pattern = "^hb1-daily-1973-1998-\\d{8}\\.xlsx$")
  )),
  hb2 = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(
    latest_file(dir = "data-raw/RBNZ", pattern = "^hb2-daily-\\d{8}\\.xlsx$"),
    latest_file(dir = "data-raw/RBNZ", pattern = "^hb2-daily-close-1985-2017.xlsx$")
  )),
  fuel = list(specific = "fuel", detectDates = TRUE, sheet = 1, startRow = 1, skipEmptyRows = TRUE, files = c("data/Fuel/fuel_data.csv"))
)




# --- helper: find the newest modification time among the source files for a dataset
.rds_path <- function(name) file.path("data/RBNZ", paste0(name, ".rds"))

.is_rds_fresh <- function(name) {
  # resolve the *current* list of source files for this dataset
  sources <- unlist(.cache$paths[[name]]["files"])
  sources <- sources[!is.na(sources) & nzchar(sources)]

  if (!length(sources)) return(FALSE)  # no sources -> can't be fresh

  # if RDS doesn't exist, not fresh
  rds_file <- .rds_path(name)
  if (!file.exists(rds_file)) return(FALSE)

  finfo <- file.info(sources)
  newest_source_time <- max(finfo$mtime, na.rm = TRUE)
  rds_time <- file.info(rds_file)$mtime

  # RDS is fresh if it's as new or newer than the newest source
  !is.na(rds_time) && rds_time >= newest_source_time
}
.load_data <- function(name, return = TRUE, refresh = "Auto", specific = NULL) {
  rds_file <- .rds_path(name)

  # Decide refresh strategy if Auto
  if (refresh == "Auto") {
    if (.is_rds_fresh(name)) {
      refresh <- "RDS"
    } else {
      refresh <- "Full"
    }
  }

  # Read cached RDS if requested/available
  if (file.exists(rds_file) && refresh == "RDS") {
    .cache$data[[name]] <- readRDS(file = rds_file)
    message("Loaded cached RDS for ", name)
  }

  # If RDS missing but user asked for RDS, fall back to Full
  if (!file.exists(rds_file) && refresh == "RDS") refresh <- "Full"

  if (refresh == "Full") {
    .cache$data[[name]] <- NULL

    .files <- unlist(.cache$paths[[name]]["files"])
    .detect_dates <- unlist(.cache$paths[[name]]["detectDates"])
    .sheet <- unlist(.cache$paths[[name]]["sheet"])
    .start_row <- as.integer(unlist(.cache$paths[[name]]["startRow"]))
    .skip_empty_rows <- unlist(.cache$paths[[name]]["skipEmptyRows"])
    .specific <- unlist(.cache$paths[[name]]["specific"])

    if (length(.files) == 0 || all(is.na(.files))) {
      stop(paste0("No files defined for data '", name, "'"))
    }

    .tempfile <- NULL  # important: initialize
    for (n in .files) {
      if (is.na(n) || !nzchar(n)) next
      if (grepl("\\.csv$", n, ignore.case = TRUE)) {
        x <- read.csv(n, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (grepl("\\.rds$", n, ignore.case = TRUE)) {
        x <- readRDS(n)
      } else if (grepl("\\.xlsx$", n, ignore.case = TRUE)) {
        x <- openxlsx::read.xlsx(
          n,
          detectDates   = isTRUE(.detect_dates),
          sheet         = .sheet,
          startRow      = .start_row,
          skipEmptyRows = isTRUE(.skip_empty_rows)
        )
      } else {
        stop(paste0("Unsupported file type for '", n, "'"))
      }
      if (is.null(.tempfile)) {
        .tempfile <- x
      } else {
        .tempfile <- dplyr::bind_rows(.tempfile, x) |> dplyr::distinct()
      }
    }
    if (.specific == "RBNZ") {
      .tempfile <- .tempfile |>
        dplyr::rename(Date = Series.Id) |>
        dplyr::mutate(Date = as.Date(Date)) |>
        dplyr::arrange(Date) |>
        date_summarise(Date)
    }
    if (.specific == "fuel") {
      .tempfile <- .tempfile |>
        dplyr::rename(Date = Month) |>
        dplyr::mutate(Date = as.Date(Date,format = "%d/%m/%Y")) |>
        dplyr::arrange(Date)
    }

    .cache$data[[name]] <- .tempfile
    dir.create(dirname(rds_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(.tempfile, file = rds_file)
    message("Saved RDS for ", name)
  }

  if (return) .cache$data[[name]]
}



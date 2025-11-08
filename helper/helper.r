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


latest_file <- function(dir = "data/RBNZ", pattern = "^hb1-daily-\\d{8}\\.xlsx$") {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) stop("No matching files in ", dir)
  files[order(files, decreasing = TRUE)][1]
}

.cache <- new.env(parent = emptyenv())
.cache$data <- list()
.cache$paths <- list(
  hm1  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data/RBNZ", pattern = "^hm1-\\d{8}\\.xlsx$"))),
  hm2  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data/RBNZ", pattern = "^hm2-\\d{8}\\.xlsx$"))),
  hm3  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data/RBNZ", pattern = "^hm3-\\d{8}\\.xlsx$"))),
  hm4  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data/RBNZ", pattern = "^hm4-\\d{8}\\.xlsx$"))),
  hm5  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data/RBNZ", pattern = "^hm5-\\d{8}\\.xlsx$"))),
  hm6  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data/RBNZ", pattern = "^hm6-\\d{8}\\.xlsx$"))),
  hm7  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data/RBNZ", pattern = "^hm7-\\d{8}\\.xlsx$"))),
  hm8  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data/RBNZ", pattern = "^hm8-\\d{8}\\.xlsx$"))),
  hm9  = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data/RBNZ", pattern = "^hm9-\\d{8}\\.xlsx$"))),
  hm10 = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data/RBNZ", pattern = "^hm10-\\d{8}\\.xlsx$"))),
  hm14 = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data/RBNZ", pattern = "^hm14-\\d{8}\\.xlsx$"))),
  hs32 = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(latest_file(dir = "data/RBNZ", pattern = "^hs32-\\d{8}\\.xlsx$"))),
  hb1 = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(
    latest_file(dir = "data/RBNZ", pattern = "^hb1-daily-\\d{8}\\.xlsx$"),
    latest_file(dir = "data/RBNZ", pattern = "^hb1-daily-1999-2017-\\d{8}\\.xlsx$"),
    latest_file(dir = "data/RBNZ", pattern = "^hb1-daily-1973-1998-\\d{8}\\.xlsx$")
  )),
  hb2 = list(specific = "RBNZ", detectDates = TRUE, sheet = "Data", startRow = 5, skipEmptyRows = TRUE, files = c(
    latest_file(dir = "data/RBNZ", pattern = "^hb2-daily-\\d{8}\\.xlsx$"),
    latest_file(dir = "data/RBNZ", pattern = "^hb2-daily-close-1985-2017.xlsx$")
  ))
)

.load_data <- function(name, return = TRUE, refresh = FALSE, specific = NULL) {
  need_reload <- is.null(.cache$data[[name]]) || isTRUE(refresh)
  if (need_reload) {

    .cache$data[[name]] <- NULL
    .files <- .cache$paths[[name]]['files'] %>% unlist()
    .detect_dates <- .cache$paths[[name]]['detectDates'] %>% unlist()
    .sheet <- .cache$paths[[name]]['sheet'] %>% unlist()
    .start_row <- as.integer(.cache$paths[[name]]['startRow'] %>% unlist())
    .skip_empty_rows <- .cache$paths[[name]]['skipEmptyRows'] %>% unlist()
    .specific <- .cache$paths[[name]]['specific'] %>% unlist()

    if (length(.files) ==0) {
      stop(paste0("No files defined for data '", name, "'"))
    }
    for (n in .files) {
      if (is.null(.cache$data[[name]])) {
        .cache$data[[name]] <- openxlsx::read.xlsx(n, detectDates = .detect_dates, sheet = .sheet, startRow = .start_row, skipEmptyRows = .skip_empty_rows)
      } else {
        .cache$data[[name]] <- .cache$data[[name]] %>% bind_rows(
          openxlsx::read.xlsx(n, detectDates = .detect_dates, sheet = .sheet, startRow = .start_row, skipEmptyRows = .skip_empty_rows)
        ) %>%
          distinct()
      }
    }
    if (.specific == "RBNZ") {
      .cache$data[[name]] <- .cache$data[[name]] %>%
        rename(Date = Series.Id) %>%
        mutate(Date = as.Date(Date)) %>%
        arrange(Date) %>%
        date_summarise(Date)
    }
  }
  if (return) {
    return(.cache$data[[name]])
  }
}

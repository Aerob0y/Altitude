download_latest_ect <- function(
  base_dir   = "app/data/ect",
  years_back = 15,
  quiet      = TRUE,
  download_latest  = TRUE
) {
  if (download_latest) {
    # 1. Ensure output directory exists
    if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)

    # 2. Get starting year & month (numeric)
    today        <- Sys.Date()
    start_year   <- as.integer(format(today, "%Y"))
    start_month  <- as.integer(format(today, "%m"))   # 1–12

    # Represent "year-month" as a single integer index
    start_index  <- start_year * 12 + (start_month - 1)
    max_steps    <- years_back * 12

    # 3. Loop backwards month by month
    for (step in 0:max_steps) {
      idx <- start_index - step

      year_num  <- idx %/% 12
      month_num <- idx %% 12 + 1         # back to 1–12

      year_str   <- sprintf("%d", year_num)
      month_name <- month.name[month_num]         # "June"
      month_slug <- tolower(month_name)           # "june"

      # Build URL in the same pattern as your example
      url <- sprintf(
        paste0(
          "https://www.stats.govt.nz/assets/Uploads/",
          "Electronic-card-transactions/Electronic-card-transactions-%s-%s/",
          "Download-data/electronic-card-transactions-%s-%s.zip"
        ),
        month_name, year_str, month_slug, year_str
      )

      destfile <- file.path(base_dir, basename(url))

      if (!quiet) message("Trying: ", url)

      # 3a. If file already exists locally, unzip and return
      if (file.exists(destfile)) {
        if (!quiet) message("Already downloaded: ", destfile)
        # Try unzipping; if it's bogus, continue searching
        ok_unzip <- TRUE
        tryCatch(
          utils::unzip(destfile, exdir = base_dir),
          error = function(e) {
            ok_unzip <<- FALSE
            if (!quiet) message("Existing ZIP invalid, deleting and continuing: ", destfile)
            file.remove(destfile)
          }
        )
        if (ok_unzip) return(invisible(destfile))
        # else keep looping
      }

      # 3b. Try to download
      ok_download <- TRUE
      tryCatch(
        utils::download.file(url, destfile, mode = "wb", quiet = quiet),
        error = function(e) {
          ok_download <<- FALSE
        },
        warning = function(w) {
          ok_download <<- FALSE
        }
      )

      # If download failed, clean up and move on
      if (!ok_download || !file.exists(destfile) || file.info(destfile)$size == 0) {
        if (file.exists(destfile)) file.remove(destfile)
        next
      }

      # 3c. Try to unzip the downloaded file
      ok_unzip <- TRUE
      tryCatch(
        utils::unzip(destfile, exdir = base_dir),
        error = function(e) {
          ok_unzip <<- FALSE
          if (!quiet) message("Downloaded file is not a valid ZIP, deleting: ", destfile)
          file.remove(destfile)
        }
      )

      if (ok_unzip) {
        if (!quiet) message("Downloaded and unzipped: ", destfile)
        return(invisible(destfile))
      }
      # if unzip failed, keep looking at earlier months
    }

    stop("No valid ECT ZIP file found going back ", years_back, " years.")
  }

  files <- fs::dir_ls("app/data/ECT/", regexp = "csv-tables.csv", type = "file")
  if (!length(files)) return(NA_character_)
  files <- files[order(files, decreasing = TRUE)][1]
  data <- read.csv(files[1])
  data <- data %>%
    rename(Date = Period) %>%
    mutate(Date = as.Date(Date, format = "%Y.%m")) %>%
    select(-c("Suppressed", "STATUS", "UNITS", "Magnitude", "Subject"))

  ect_values_division <- data %>%
    filter(Group == "Total values - Electronic card transactions A/S/T by division")
  saveRDS(ect_values_division, file = "app/data/ECT/ect_values_division.rds", compress = FALSE)
  ect_values_industry <- data %>%
    filter(Group == "Values - Electronic card transactions A/S/T by industry group")
  saveRDS(ect_values_industry, file = "app/data/ECT/ect_values_industry.rds", compress = FALSE)
  ect_number <- data %>%
    filter(Group == "Number of electronic card transactions A/S/T by division")
  saveRDS(ect_number, file = "app/data/ECT/ect_number.rds", compress = FALSE)
  ect_mean <- data %>%
    filter(Group == "Electronic card transactions by mean and proportion")
  saveRDS(ect_mean, file = "app/data/ECT/ect_mean.rds", compress = FALSE)
  ect_totals_percentage_changes <- data %>%
    filter(Group == "Totals - Electronic card transactions by division, percentage changes")
  saveRDS(ect_totals_percentage_changes, file = "app/data/ECT/ect_totals_percentage_changes.rds", compress = FALSE)
}
download_latest_ect()

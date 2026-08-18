download_latest_ect <- function(
  base_dir        = "app/data/ECT",
  years_back      = 15,
  quiet           = TRUE,
  download_latest = TRUE
) {

  # ---------------------------------------------------------------------------
  # PURPOSE
  #
  # Downloads the latest available Stats NZ Electronic Card Transactions (ECT)
  # ZIP file, unzips it, finds the main CSV table, and creates several RDS files
  # used by the application.
  #
  # The function searches backwards month-by-month until it finds a valid ZIP.
  #
  # Set download_latest = FALSE to skip the download and rebuild the RDS files
  # from the CSV already stored in base_dir.
  # ---------------------------------------------------------------------------


  # ===========================================================================
  # 1. DOWNLOAD LATEST AVAILABLE ECT FILE
  # ===========================================================================

  if (download_latest) {

    # Ensure the ECT data directory exists.
    if (!dir.exists(base_dir)) {
      dir.create(base_dir, recursive = TRUE)
    }

    # Start searching from the current month.
    today       <- Sys.Date()
    start_year  <- as.integer(format(today, "%Y"))
    start_month <- as.integer(format(today, "%m"))

    # Convert year/month into a single monthly index.
    # This makes stepping backwards across years straightforward.
    start_index <- start_year * 12 + (start_month - 1)

    max_steps <- years_back * 12

    found_file <- FALSE
    zip_file   <- NULL


    # Search backwards month-by-month until a valid Stats NZ ZIP is found.
    for (step in 0:max_steps) {

      idx <- start_index - step

      year_num  <- idx %/% 12
      month_num <- idx %% 12 + 1

      year_str   <- sprintf("%d", year_num)
      month_name <- month.name[month_num]
      month_slug <- tolower(month_name)


      # Stats NZ ECT URL structure.
      url <- sprintf(
        paste0(
          "https://www.stats.govt.nz/assets/Uploads/",
          "Electronic-card-transactions/Electronic-card-transactions-%s-%s/",
          "Download-data/electronic-card-transactions-%s-%s.zip"
        ),
        month_name,
        year_str,
        month_slug,
        year_str
      )

      destfile <- file.path(base_dir, basename(url))

      if (!quiet) {
        message("Trying: ", url)
      }


      # -----------------------------------------------------------------------
      # 1A. FILE ALREADY EXISTS LOCALLY
      # -----------------------------------------------------------------------

      if (file.exists(destfile)) {

        if (!quiet) {
          message("Already downloaded: ", destfile)
        }

        ok_unzip <- TRUE

        tryCatch(
          utils::unzip(destfile, exdir = base_dir),

          error = function(e) {
            ok_unzip <<- FALSE

            if (!quiet) {
              message(
                "Existing ZIP invalid. Deleting and continuing: ",
                destfile
              )
            }

            file.remove(destfile)
          }
        )

        if (ok_unzip) {
          found_file <- TRUE
          zip_file   <- destfile
          break
        }
      }


      # -----------------------------------------------------------------------
      # 1B. TRY DOWNLOADING FILE
      # -----------------------------------------------------------------------

      ok_download <- TRUE

      tryCatch(
        utils::download.file(
          url,
          destfile,
          mode  = "wb",
          quiet = quiet
        ),

        error = function(e) {
          ok_download <<- FALSE
        },

        warning = function(w) {
          ok_download <<- FALSE
        }
      )


      # Download failed or created an empty file.
      if (
        !ok_download ||
        !file.exists(destfile) ||
        file.info(destfile)$size == 0
      ) {

        if (file.exists(destfile)) {
          file.remove(destfile)
        }

        next
      }


      # -----------------------------------------------------------------------
      # 1C. CHECK THAT THE DOWNLOADED FILE IS A VALID ZIP
      # -----------------------------------------------------------------------

      ok_unzip <- TRUE

      tryCatch(
        utils::unzip(destfile, exdir = base_dir),

        error = function(e) {
          ok_unzip <<- FALSE

          if (!quiet) {
            message(
              "Downloaded file is not a valid ZIP. Deleting: ",
              destfile
            )
          }

          file.remove(destfile)
        }
      )


      if (ok_unzip) {

        found_file <- TRUE
        zip_file   <- destfile

        if (!quiet) {
          message("Downloaded and unzipped: ", destfile)
        }

        break
      }
    }


    # Nothing valid was found within the search period.
    if (!found_file) {
      stop(
        "No valid ECT ZIP file found going back ",
        years_back,
        " years."
      )
    }

    message(
      "ECT download successful: ",
      basename(zip_file)
    )
  }


  # ===========================================================================
  # 2. FIND THE LATEST EXTRACTED CSV
  # ===========================================================================

  files <- fs::dir_ls(
    base_dir,
    regexp = "csv-tables.csv$",
    type   = "file"
  )

  if (!length(files)) {
    stop("No ECT csv-tables.csv file found in: ", base_dir)
  }


  # If multiple months have been extracted, use the latest filename.
  latest_file <- files[
    which.max(
      as.Date(
        paste0(
          "01-",
          stringr::str_extract(
            files,
            "(?i)(january|february|march|april|may|june|july|august|september|october|november|december)-\\d{4}"
          )
        ),
        format = "%d-%B-%Y"
      )
    )
  ]

  message(
    "Processing ECT file: ",
    basename(latest_file)
  )

  # ===========================================================================
  # 3. READ AND CLEAN DATA
  # ===========================================================================

  data <- read.csv(
    latest_file,
    check.names = FALSE
  )

  data <- data %>%
    rename(Date = Period) %>%
    mutate(
      Date = as.Date(
        paste0(as.character(data$Period), ".01"),
        format = "%Y.%m.%d"
      )
    ) %>%
    select(
      -c(
        Suppressed,
        STATUS,
        UNITS,
        Magnitude,
        Subject
      )
    )


  # ===========================================================================
  # 4. CREATE DATASETS USED BY THE APPLICATION
  # ===========================================================================


  # Total transaction values by division.
  ect_values_division <- data %>%
    filter(
      Group ==
        "Total values - Electronic card transactions A/S/T by division"
    )

  saveRDS(
    ect_values_division,
    file = file.path(base_dir, "ect_values_division.rds"),
    compress = FALSE
  )


  # Transaction values by industry group.
  ect_values_industry <- data %>%
    filter(
      Group ==
        "Values - Electronic card transactions A/S/T by industry group"
    )

  saveRDS(
    ect_values_industry,
    file = file.path(base_dir, "ect_values_industry.rds"),
    compress = FALSE
  )


  # Number of electronic card transactions.
  ect_number <- data %>%
    filter(
      Group ==
        "Number of electronic card transactions A/S/T by division"
    )

  saveRDS(
    ect_number,
    file = file.path(base_dir, "ect_number.rds"),
    compress = FALSE
  )


  # Mean transaction values and proportions.
  ect_mean <- data %>%
    filter(
      Group ==
        "Electronic card transactions by mean and proportion"
    )

  saveRDS(
    ect_mean,
    file = file.path(base_dir, "ect_mean.rds"),
    compress = FALSE
  )


  # Percentage changes in total transactions.
  ect_totals_percentage_changes <- data %>%
    filter(
      Group ==
        "Totals - Electronic card transactions by division, percentage changes"
    )

  saveRDS(
    ect_totals_percentage_changes,
    file = file.path(
      base_dir,
      "ect_totals_percentage_changes.rds"
    ),
    compress = FALSE
  )


  # ===========================================================================
  # 5. SUCCESS MESSAGE
  # ===========================================================================

  message(
    paste0(
      "\nECT update successfully completed.\n",
      "Source file: ", basename(latest_file), "\n",
      "Latest data date: ", max(data$Date, na.rm = TRUE), "\n",
      "Rows processed: ", format(nrow(data), big.mark = ","), "\n",
      "RDS files written to: ", base_dir
    )
  )

  invisible(TRUE)
}

temp <- download_latest_ect()

head(temp)

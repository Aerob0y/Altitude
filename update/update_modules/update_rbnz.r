# ==============================================================================
# README -----------------------------------------------------------------------
# Purpose: ----------------
# Download and process Reserve Bank of New Zealand (RBNZ) statistics Excel files.
# Downloads selected Reserve Bank of New Zealand (RBNZ) statistics Excel files,
# saves dated local copies, and converts the "Data" worksheet into an RDS file
# for use elsewhere in the project.
#
# Main function----------------
# rbnz_fetch_xlsx() 
#
# Example:
#
#   rbnz_fetch_xlsx("hb1-daily")
#
#   rbnz_fetch_xlsx(
#     keys = c("hb1-daily", "hb2-daily", "hm1")
#   )
#
# Output
# Files are stored by default in:
#
#   app/data/RBNZ/
#
# Two outputs are created for each successful download:
#
#   1. A dated copy of the original Excel file:
#        hb1-daily-YYYYMMDD.xlsx
#
#   2. A processed R data file:
#        hb1-daily.rds
#
# The RDS file contains the RBNZ "Data" worksheet, with the first column renamed
# to Date and converted to an R Date.
#
# Notes ----------------
# HTTP ETag and Last-Modified values are stored in:
#
#   app/data/RBNZ/_meta/
#
# These values are sent with later requests so that RBNZ can return HTTP 304
# when the source file has not changed, avoiding unnecessary downloads.
#
# If a download fails or RBNZ returns HTTP 304, the function returns the path
# to the most recent dated Excel file already available locally.
#
# Helper functions
#
# rbnz_latest_local()             - Finds the most recent dated Excel file for an RBNZ series.
# rbnz_create_handle()            - Creates and configures the curl HTTP connection.
# rbnz_add_conditional_headers()  - Adds stored ETag and Last-Modified headers to a request.
# rbnz_save_metadata()            - Saves HTTP metadata returned by RBNZ for future conditional requests.
# rbnz_save_download()            - Saves the downloaded Excel file and creates the corresponding RDS file.
# rbnz_result()                   - Creates a standard result object for each requested series.
# rbnz_fetch_one()                - Handles the download and processing of a single RBNZ series.
#
# Dependencies ------------
# fs
# curl
# openxlsx
# dplyr
# here
#
# Notes -----
# - Paths are relative to the project root.
# - Valid RBNZ series and their source URLs are defined in `rbnz_urls`.
# - Unknown series keys will cause rbnz_fetch_xlsx() to stop with an error.
# - Set verbose = FALSE to suppress status messages.
# - Set http2 = FALSE if HTTP/2 causes problems on the current network.
#
# ==============================================================================

library(here)


rbnz_urls <- c(
  "hb1-daily" = "https://www.rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/b/b1/hb1-daily.xlsx",
  "hb1-daily-1999-2017" = "https://www.rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/b/b1/hb1-daily-1999-2017.xlsx",
  "hb1-daily-1973-1998" = "https://www.rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/b/b1/hb1-daily-1973-1998.xlsx",
  "hb1-monthly" = "https://www.rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/b/b1/hb1-monthly.xlsx",
  "hb1-monthly-1973-1998" = "https://www.rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/b/b1/hb1-monthly-1973-1998.xlsx",
  "hb2-daily" = "https://www.rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/b/b2/hb2-daily-close.xlsx",
  "hc35" = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/c/c35/hc35.xlsx",
  "hm1" = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m1/hm1.xlsx",
  "hm2" = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m2/hm2.xlsx",
  "hm3" = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m3/hm3.xlsx",
  "hm4" = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m4/hm4.xlsx",
  "hm5" = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m5/hm5.xlsx",
  "hm6" = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m6/hm6.xlsx",
  "hm7" = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m7/hm7.xlsx",
  "hm8" = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m8/hm8.xlsx",
  "hm9" = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m9/hm9.xlsx",
  "hm10" = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m10/hm10.xlsx",
  "hm14" = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m14/hm14.xlsx",
  "hs32" = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/l-s/s32/hs32.xlsx"
)

rbnz_latest_local <- function(key, out_dir) {
  pattern <- paste0("^", key, "-\\d{8}\\.xlsx$")

  files <- fs::dir_ls(
    out_dir,
    regexp = pattern,
    type = "file"
  )

  if (length(files) == 0) {
    return(NA_character_)
  }

  sort(files, decreasing = TRUE)[1]
}

rbnz_create_handle <- function(referer, http2 = TRUE) {
  ua <- paste(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
    "AppleWebKit/537.36 (KHTML, like Gecko)",
    "Chrome/120 Safari/537.36"
  )

  h <- curl::new_handle()

  curl::handle_setopt(
    h,
    followlocation = TRUE,
    http_version = if (http2) 2L else 1L,
    ssl_verifypeer = TRUE,
    ssl_verifyhost = 2L
  )

  curl::handle_setheaders(
    h,
    "User-Agent" = ua,
    "Referer" = referer,
    "Accept" = "*/*",
    "Accept-Language" = "en-US,en;q=0.9",
    "Connection" = "keep-alive"
  )

  h
}

# rbnz_add_conditional_headers() is a function that adds conditional headers (If-None-Match and If-Modified-Since) to a curl handle based on the metadata files for a given key. It takes three parameters: handle (the curl handle), key (the key corresponding to the RBNZ data file), and meta_dir (the directory containing the metadata files). The function returns the modified curl handle.
rbnz_add_conditional_headers <- function(
  handle,
  key,
  meta_dir
) {
  etag_file <- file.path(meta_dir, paste0(key, ".etag"))
  last_file <- file.path(meta_dir, paste0(key, ".last"))

  if (fs::file_exists(etag_file)) {
    curl::handle_setheaders(
      handle,
      "If-None-Match" = readLines(etag_file, n = 1)
    )
  }

  if (fs::file_exists(last_file)) {
    curl::handle_setheaders(
      handle,
      "If-Modified-Since" = readLines(last_file, n = 1)
    )
  }

  invisible(handle)
}

# rbnz_save_metadata() is a function that saves the metadata (ETag and Last-Modified headers) for a given RBNZ data file. It takes three parameters: headers (the HTTP response headers), key (the key corresponding to the RBNZ data file), and meta_dir (the directory to save the metadata files). The function returns NULL.
rbnz_save_metadata <- function(
  headers,
  key,
  meta_dir
) {
  hdrs <- curl::parse_headers_list(headers)

  if (!is.null(hdrs$etag)) {
    writeLines(
      hdrs$etag,
      file.path(meta_dir, paste0(key, ".etag"))
    )
  }

  if (!is.null(hdrs$`last-modified`)) {
    writeLines(
      hdrs$`last-modified`,
      file.path(meta_dir, paste0(key, ".last"))
    )
  }

  invisible(NULL)
}

# rbnz_save_download() is a function that saves the downloaded RBNZ data file to the specified output directory. It takes three parameters: res (the HTTP response object), key (the key corresponding to the RBNZ data file), and out_dir (the directory to save the downloaded file). The function returns the path to the saved XLSX file.
rbnz_save_download <- function(
  res,
  key,
  out_dir
) {
  xlsx_path <- file.path(
    out_dir,
    sprintf(
      "%s-%s.xlsx",
      key,
      format(Sys.Date(), "%Y%m%d")
    )
  )

  tmp <- fs::file_temp(ext = "xlsx")
  on.exit(fs::file_delete(tmp), add = TRUE)

  writeBin(res$content, tmp)

  fs::file_copy(
    tmp,
    xlsx_path,
    overwrite = TRUE
  )

  data <- openxlsx::read.xlsx(
    tmp,
    detectDates = TRUE,
    sheet = "Data",
    startRow = 5,
    skipEmptyRows = TRUE
  ) |>
    dplyr::rename(Date = 1) |>
    dplyr::mutate(Date = as.Date(Date))

  saveRDS(
    data,
    file = file.path(out_dir, paste0(key, ".rds")),
    compress = FALSE
  )

  xlsx_path
}

# rbnz_result() is a function that creates a result object for an RBNZ data file download operation. It takes four parameters: key (the key corresponding to the RBNZ data file), status (the HTTP status code of the download operation), downloaded (a boolean indicating whether the file was downloaded), and path (the path to the downloaded or latest local file). The function returns a list containing these values.
rbnz_result <- function(
  key,
  status,
  downloaded,
  path
) {
  list(
    key = key,
    status = status,
    downloaded = downloaded,
    path = path
  )
}

rbnz_fetch_one <- function(
  key,
  url,
  handle,
  out_dir,
  meta_dir,
  verbose = TRUE
) {
  rbnz_add_conditional_headers(
    handle,
    key,
    meta_dir
  )

  res <- try(
    curl::curl_fetch_memory(
      url,
      handle = handle
    ),
    silent = TRUE
  )

  if (inherits(res, "try-error")) {
    if (verbose) {
      message(
        sprintf("[RBNZ:%s] ERROR: %s", key, res)
      )
    }

    return(
      rbnz_result(
        key = key,
        status = "error",
        downloaded = FALSE,
        path = rbnz_latest_local(key, out_dir)
      )
    )
  }

  if (res$status_code == 304) {
    if (verbose) {
      message(
        sprintf("[RBNZ:%s] Up-to-date (304).", key)
      )
    }

    return(
      rbnz_result(
        key = key,
        status = 304,
        downloaded = FALSE,
        path = rbnz_latest_local(key, out_dir)
      )
    )
  }

  if (res$status_code < 200 || res$status_code >= 300) {
    if (verbose) {
      message(
        sprintf(
          "[RBNZ:%s] HTTP %s (no file).",
          key,
          res$status_code
        )
      )
    }

    return(
      rbnz_result(
        key = key,
        status = res$status_code,
        downloaded = FALSE,
        path = rbnz_latest_local(key, out_dir)
      )
    )
  }

  path <- rbnz_save_download(
    res,
    key,
    out_dir
  )

  rbnz_save_metadata(
    res$headers,
    key,
    meta_dir
  )

  if (verbose) {
    message(
      sprintf(
        "[RBNZ:%s] Downloaded -> %s",
        key,
        path
      )
    )
  }

  rbnz_result(
    key = key,
    status = res$status_code,
    downloaded = TRUE,
    path = path
  )
}

rbnz_fetch_xlsx <- function(
  keys = c("hb1-daily"),
  out_dir = "app/data/RBNZ",
  referer = "https://www.rbnz.govt.nz/statistics/series/data-file-index-page",
  verbose = TRUE,
  http2 = TRUE
) {
  fs::dir_create(out_dir)

  meta_dir <- file.path(out_dir, "_meta")
  fs::dir_create(meta_dir)

  bad <- setdiff(keys, names(rbnz_urls))

  if (length(bad) > 0) {
    stop(
      "Unknown key(s): ",
      paste(bad, collapse = ", ")
    )
  }

  handle <- rbnz_create_handle(
    referer,
    http2
  )

  try(
    suppressWarnings(
      curl::curl_fetch_memory(
        referer,
        handle = handle
      )
    ),
    silent = TRUE
  )

  results <- lapply(
    keys,
    function(key) {
      rbnz_fetch_one(
        key = key,
        url = rbnz_urls[[key]],
        handle = handle,
        out_dir = out_dir,
        meta_dir = meta_dir,
        verbose = verbose
      )
    }
  )

  paths <- vapply(
    results,
    function(x) x$path,
    character(1)
  )

  names(paths) <- vapply(
    results,
    `[[`,
    character(1),
    "key"
  )

  if (verbose) {
    print(paths)
  }

  invisible(paths)
}

rbnz_fetch_all <- function() {
  rbnz_fetch_xlsx(keys = c(
    "hb1-daily",
    "hb2-daily",
    "hc35",
    "hm1",
    "hm2",
    "hm3",
    "hm4",
    "hm5",
    "hm6",
    "hm7",
    "hm8",
    "hm9",
    "hm10",
    "hm14",
    "hs32"
  ))
}

update <- rbnz_fetch_all
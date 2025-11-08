# install.packages(c("curl","fs"))
library(curl)
library(fs)

rbnz_fetch_xlsx <- function(
  keys      = c("hb1-daily"),     # or c("hb1-daily","hb1-daily-1999-2017","hb1-daily-1973-1998")
  out_dir   = "data/RBNZ",
  referer   = "https://www.rbnz.govt.nz/statistics/series/data-file-index-page",
  verbose   = TRUE,
  http2     = TRUE                # set FALSE to force HTTP/1.1 if your network blocks h2
) {
  dir_create(out_dir)
  meta_dir <- file.path(out_dir, "_meta"); dir_create(meta_dir)

  # Map keys -> exact WWW URLs (match the href targets you saw)
  urls <- c(
    "hb1-daily"          = "https://www.rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/b/b1/hb1-daily.xlsx",
    "hb1-daily-1999-2017"= "https://www.rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/b/b1/hb1-daily-1999-2017.xlsx",
    "hb1-daily-1973-1998"= "https://www.rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/b/b1/hb1-daily-1973-1998.xlsx",
    "hb1-monthly"        = "https://www.rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/b/b1/hb1-monthly.xlsx",
    "hb1-monthly-1973-1998" = "https://www.rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/b/b1/hb1-monthly-1973-1998.xlsx",
    "hb2-daily"          = "https://www.rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/b/b2/hb2-daily-close.xlsx",
    "hc35"               = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/c/c35/hc35.xlsx",
    "hm1"                = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m1/hm1.xlsx",
    "hm2"                = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m2/hm2.xlsx",
    "hm3"                = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m3/hm3.xlsx",
    "hm4"                = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m4/hm4.xlsx",
    "hm5"                = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m5/hm5.xlsx",
    "hm6"                = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m6/hm6.xlsx",
    "hm7"                = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m7/hm7.xlsx",
    "hm8"                = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m8/hm8.xlsx",
    "hm9"                = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m9/hm9.xlsx",
    "hm10"               = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m10/hm10.xlsx",
    "hm14"               = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/m/m14/hm14.xlsx",
    "hs32"               = "https://rbnz.govt.nz/-/media/project/sites/rbnz/files/statistics/series/l-s/s32/hs32.xlsx"
  )

  bad <- setdiff(keys, names(urls))
  if (length(bad)) stop("Unknown key(s): ", paste(bad, collapse = ", "))

  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120 Safari/537.36"
  etag_path <- function(key) file.path(meta_dir, paste0(key, ".etag"))
  last_path <- function(key) file.path(meta_dir, paste0(key, ".last"))

  latest_local <- function(key) {
    patt <- paste0("^", key, "-\\d{8}\\.xlsx$")
    files <- dir_ls(out_dir, regexp = patt, type = "file")
    if (!length(files)) return(NA_character_)
    files[order(files, decreasing = TRUE)][1]
  }

  # One handle reused for all requests (keeps cookies)
  h <- new_handle()
  handle_setopt(h,
    followlocation = TRUE,
    http_version   = if (http2) 2L else 1L,
    ssl_verifypeer = TRUE,
    ssl_verifyhost = 2L
  )
  handle_setheaders(h,
    "User-Agent"      = ua,
    "Referer"         = referer,
    "Accept"          = "*/*",
    "Accept-Language" = "en-US,en;q=0.9",
    "Connection"      = "keep-alive"
  )
  # Warm-up fetch to set cookies
  try(suppressWarnings(curl_fetch_memory(referer, handle = h)), silent = TRUE)

  fetch_one <- function(key) {
    url <- urls[[key]]
    # Reset base headers (avoid stacking conditional headers across keys)
    handle_setheaders(h,
      "User-Agent"      = ua,
      "Referer"         = referer,
      "Accept"          = "*/*",
      "Accept-Language" = "en-US,en;q=0.9",
      "Connection"      = "keep-alive"
    )
    # Conditional headers if present
    if (file_exists(etag_path(key))) handle_setheaders(h, "If-None-Match"     = readLines(etag_path(key), 1))
    if (file_exists(last_path(key))) handle_setheaders(h, "If-Modified-Since" = readLines(last_path(key), 1))

    res <- try(curl_fetch_memory(url, handle = h), silent = TRUE)
    if (inherits(res, "try-error")) {
      if (verbose) message(sprintf("[HB1:%s] ERROR: %s", key, res))
      # Fall back to latest local (if any)
      return(list(
        key = key, status = "error", downloaded = FALSE,
        path = latest_local(key)
      ))
    }

    if (res$status_code == 304) {
      if (verbose) message(sprintf("[HB1:%s] Up-to-date (304).", key))
      return(list(
        key = key, status = 304, downloaded = FALSE,
        path = latest_local(key)
      ))
    }

    if (res$status_code >= 200 && res$status_code < 300) {
      fn  <- file.path(out_dir, sprintf("%s-%s.xlsx", key, format(Sys.Date(), "%Y%m%d")))
      tmp <- file_temp(ext = "xlsx")
      writeBin(res$content, tmp)
      file_copy(tmp, fn, overwrite = TRUE)
      file_delete(tmp)

      hdrs <- parse_headers_list(res$headers)
      if (!is.null(hdrs$etag))            writeLines(hdrs$etag, etag_path(key))
      if (!is.null(hdrs$`last-modified`)) writeLines(hdrs$`last-modified`, last_path(key))

      if (verbose) message(sprintf("[HB1:%s] Downloaded -> %s", key, fn))
      return(list(
        key = key, status = res$status_code, downloaded = TRUE,
        path = fn
      ))
    }

    if (verbose) message(sprintf("[HB1:%s] HTTP %s (no file).", key, res$status_code))
    list(
      key = key, status = res$status_code, downloaded = FALSE,
      path = latest_local(key)
    )
  }

  out <- lapply(keys, fetch_one)

  # Return a named vector of latest file paths (NA if none available yet)
  paths <- vapply(out, function(x) if (length(x$path)) x$path else NA_character_, character(1))
  names(paths) <- vapply(out, `[[`, character(1), "key")
  if (verbose) print(paths)
  invisible(paths)


}

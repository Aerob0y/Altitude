download_if_updated_etag <- function(url, dest_file, etag_file = paste0(dest_file, ".etag")) {
  req <- httr2::request(url)
  # Add If-None-Match header if we have a stored ETag
  if (file.exists(etag_file)) {
    etag <- readLines(etag_file, warn = FALSE)
    if (nzchar(etag)) {
      req <- httr2::req_headers(req, "If-None-Match" = etag)
    }
  }
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (status == 304) {
    message("No update (HTTP 304 Not Modified).")
    return(invisible(FALSE))
  }
  if (status >= 200 && status < 300) {
    # Save body
    writeBin(httr2::resp_body_raw(resp), dest_file)
    # Save new ETag if present
    hdrs <- httr2::resp_headers(resp)
    if (!is.null(hdrs$etag)) {
      writeLines(hdrs$etag, etag_file)
    }
    message("Downloaded updated JSON.")
    return(invisible(TRUE))
  }
  stop("Request failed with status ", status)
}

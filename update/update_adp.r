library(jsonlite)
library(tidyverse)
library(readxl)
library(sf)
library(svglite)
library(request)
library(httr2)


download_if_updated_etag <- function(url, dest_file, etag_file = paste0(dest_file, ".etag")) {
  req <- request(url)
  
  # Add If-None-Match header if we have a stored ETag
  if (file.exists(etag_file)) {
    etag <- readLines(etag_file, warn = FALSE)
    if (nzchar(etag)) {
      req <- req_headers(req, "If-None-Match" = etag)
    }
  }
  
  resp <- req_perform(req)
  
  status <- resp_status(resp)
  
  if (status == 304) {
    message("No update (HTTP 304 Not Modified).")
    return(invisible(FALSE))
  }
  
  if (status >= 200 && status < 300) {
    # Save body
    writeBin(resp_body_raw(resp), dest_file)
    
    # Save new ETag if present
    hdrs <- resp_headers(resp)
    if (!is.null(hdrs$etag)) {
      writeLines(hdrs$etag, etag_file)
    }
    
    message("Downloaded updated JSON.")
    return(invisible(TRUE))
  }
  
  stop("Request failed with status ", status)
}

update_adp_data <- function() {
  download_if_updated_etag("https://teic.mbie.govt.nz/ste/data/views/theEconomy/economicResilience/adpByRTO.json", "data/ADP/adpByRTO.json")
}


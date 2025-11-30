library(jsonlite)
library(tidyverse)
library(readxl)
library(sf)
library(svglite)
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

coverted_regions <- data.frame(
  Original = c("Auckland", "Tātaki Auckland Unlimited", "Bay of Plenty", "Tourism Bay of Plenty", "Canterbury", "Combined Canterbury RTOs", "ChristchurchNZ", "Central Otago", "Tourism Central Otago", "Clutha", "Clutha Development", "Coromandel", "Destination Coromandel", "Dunedin", "Enterprise Dunedin", "Fiordland", "Visit Fiordland", "Gisborne", "Tairāwhiti Gisborne Tourism", "Hawke's Bay", "Hawke's Bay Tourism", "Hurunui", "Hurunui Tourism", "Destination Kaikōura", "Kapiti-Horowhenua", "Mackenzie", "Mackenzie Tourism", "Manawatu", "CEDA (Manawatū)", "Marlborough", "Destination Marlborough", "Nelson-Tasman", "NRDA (Nelson)", "Northland", "Northland Inc", "Queenstown", "Destination Queenstown", "Rotorua", "RotoruaNZ", "Ruapehu", "Visit Ruapehu", "Southland", "Visit Southland", "Taranaki", "Venture Taranaki", "Taupo", "Destination Great Lake Taupō", "Timaru", "Venture Timaru", "Total NZ", "New Zealand", "Waikato", "Hamilton & Waikato Tourism", "Wairarapa", "Destination Wairarapa", "Waitaki", "Tourism Waitaki", "Wanaka", "Lake Wānaka Tourism", "Wanganui", "Whanganui and Partners", "Wellington", "WellingtonNZ", "Wellington City", "West Coast", "Development West Coast", "Whakatane-Kawerau"),
  region_new = c("Auckland", "Auckland", "Bay of Plenty", "Bay of Plenty", "Canterbury", "Canterbury", "Canterbury", "Central Otago", "Central Otago", "Clutha", "Clutha", "Coromandel", "Coromandel", "Dunedin", "Dunedin", "Fiordland", "Fiordland", "Gisborne", "Gisborne", "Hawke's Bay", "Hawke's Bay", "Hurunui", "Hurunui", "Kaikōura", "Kapiti-Horowhenua", "Mackenzie", "Mackenzie", "Manawatu", "Manawatu", "Marlborough", "Marlborough", "Nelson-Tasman", "Nelson-Tasman", "Northland", "Northland", "Queenstown", "Queenstown", "Rotorua", "Rotorua", "Ruapehu", "Ruapehu", "Southland", "Southland", "Taranaki", "Taranaki", "Taupo", "Taupo", "Timaru", "Timaru", "Total", "Total", "Waikato", "Waikato", "Wairarapa", "Wairarapa", "Waitaki", "Waitaki", "Wanaka", "Wanaka", "Wanganui", "Wanganui", "Wellington", "Wellington", "Wellington City", "West Coast", "West Coast", "Whakatane-Kawerau")
)


rds_adp_data <- function() {
x <- fromJSON("data/ADP/adpByRTO.json", simplifyVector = FALSE)
  has_data <- purrr::map_lgl(x, ~ !is.null(.x$data))
  if (!any(has_data)) stop("No 'data' node found in the JSON.")
  holder <- x[[which(has_data)[1]]]$data

  out <- purrr::map_dfr(holder, function(pt) {
    property_type <- pt$PropertyType
    purrr::map_dfr(pt$data, function(m) {
      measure <- m$Measure
      rows <- m$data
      df <- purrr::map_dfr(rows, ~ as_tibble(.x, .name_repair = "minimal"))
      df %>%
        pivot_longer(
          cols = -"TimePeriod",
          names_to = "Region",
          values_to = "Value"
        ) %>%
        mutate(
          PropertyType = property_type,
          Measure      = measure,
          Date         = as.Date(TimePeriod),
          .before = 1
        ) %>%
        select(PropertyType, Measure, Date, Region, Value)
    })
  })
  out <- out %>%
    pivot_wider(names_from = Measure, values_from = Value) %>%
    arrange(Date, Region, PropertyType)
  historic <- read.csv("data/ADP/adp_historic.csv", check.names=FALSE)
  historic$Date <- as.Date(historic$Date, format="%d/%m/%Y")
  historic <- historic %>%
    mutate(`Number of establishments` = as.numeric(`Number of establishments`),
    `Daily Capacity (stay-units available)` = as.numeric(`Daily Capacity (stay-units available)`),
    `Available monthly stay unit 0apa0ity` = as.numeric(`Available monthly stay unit 0apa0ity`),
    `Occupancy rate (%)` = as.numeric(`Occupancy rate (%)`),
    `Total guest nights` = as.numeric(`Total guest nights`),
    `Guest arrivals` = as.numeric(`Guest arrivals`),
    `Stay unit nights occupied` = as.numeric(`Stay unit nights occupied`),
    `Domestic guest nights` = as.numeric(`Domestic guest nights`),
    `International guest nights` = as.numeric(`International guest nights`)
    )
  out <- bind_rows(historic, out) %>%
    distinct()
  out <- out %>%
    left_join(coverted_regions, by = c("Region" = "Original")) %>%
    select(Date, region_new, PropertyType, everything(), -Region) %>%
    rename("Regions" = "region_new")
  out$PropertyType[out$PropertyType == "All Property Types"] <- "All"
  out %>%
    saveRDS("data/ADP/adpByRTO.rds")
}
rds_adp_data()

update_adp_data <- function() {
  download_if_updated_etag("https://teic.mbie.govt.nz/ste/data/views/theEconomy/economicResilience/adpByRTO.json", "data/ADP/adpByRTO.json")
  rds_adp_data()
}

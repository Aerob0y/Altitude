update_fuel_data <- function(location = "app/data/Fuel/fuel_data.csv", n = 12, run = FALSE) {

  # Only proceed if file is missing OR last modified > 3 days ago
  should_run <- !file.exists(location) ||
    (as.POSIXct(Sys.time()) - file.info(location)$mtime) > as.difftime(3, units = "days") ||
    run

  if (!should_run) return(invisible(NULL))

  # If the file exists, load it; otherwise start from an empty frame
  if (file.exists(location)) {
    current_data <- read.csv(location) %>%
      mutate(Date = as.Date(Date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))) %>%
      select(Date, Price)
  } else {
    current_data <- data.frame(Date = as.Date(character()), Price = numeric())
  }

  url <- paste0("https://www.indexmundi.com/commodities/?commodity=jet-fuel&months=", as.character(n))
  webpage <- rvest::read_html(url)

  updated_data <- webpage %>%
    html_node("#gvPrices") %>%
    html_table() %>%
    mutate(Month = as.Date(paste(Month, "01"), format = "%b %Y %d")) %>%
    mutate(Price = Price / 0.0238095238) %>%
    rename(Date = Month) %>%
    select(Date, Price)

  current_data <- current_data %>% filter(!(Date %in% updated_data$Date))
  updated_data <- rbind(current_data, updated_data)

  write.csv(updated_data, location, row.names = FALSE)

  nzd_usd <- load_data("hb1") %>%
    select(Date, EXR.DS11.D06) %>%
    rename(USD_NZD = EXR.DS11.D06)

  result <- merge(updated_data, nzd_usd, by = "Date", all.x = TRUE) %>%
    arrange(Date) %>%
    tidyr::fill(USD_NZD, .direction = "down") %>%
    dplyr::rename(`USD per Barrel` = Price) %>%
    filter(!is.na(USD_NZD)) %>%
    mutate(`NZD per Barrel` = (`USD per Barrel` / USD_NZD))

  saveRDS(result, file = "app/data/Fuel/fuel.rds", compress = FALSE)

  invisible(result)
}



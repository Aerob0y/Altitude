
# Library -----------------------------------------------------------------


library(tidyverse)
library(rvest)
library(lubridate)

# updateFuelData -----------------------------------------------------------------

update_fuel_data <- function(location="data/Fuel/fuel_data.csv", n=6){
  current_data <- read.csv(location) %>%
    mutate(Date = as.Date(Date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))) %>% 
    select(Date, Price)
  url <- paste("https://www.indexmundi.com/commodities/?commodity=jet-fuel&months=", as.character(n), sep = "")
  webpage <- rvest::read_html(url)
  updated_data <- webpage  %>% html_node("#gvPrices") |>  # Replace "table" with the specific CSS selector if necessary
    html_table() %>%
    mutate(Month =  as.Date(paste(Month, " 01"), format = "%b %Y %d")) %>%
    mutate(Price = Price / 0.0238095238) %>%
    rename(Date = Month) %>%
    select(Date, Price)
  current_data <- current_data %>% filter(!(Date %in% updated_data$Date))
  updated_data <- rbind(current_data, updated_data)
  write.csv(updated_data, location, row.names = FALSE)

  nzd_usd <- load_data("hb1") %>% select(Date, EXR.DS11.D06) %>% rename(Date = Date, USD_NZD = EXR.DS11.D06)
  result <- merge(updated_data, nzd_usd, by = 'Date', all.x = TRUE)
  result <- arrange(result, Date)
  result <- fill(result, USD_NZD, .direction = "down")
  result <- rename(result, c("USD per Barrel" = "Price"))
  result <- filter(result, !is.na(USD_NZD))
  result <- result %>% mutate('NZD per Barrel' = (`USD per Barrel`/ USD_NZD))
  saveRDS(result, file = "data/fuel/fuel.rds", compress = FALSE)
}

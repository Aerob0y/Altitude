
# Library -----------------------------------------------------------------


library(tidyverse)
library(rvest)
library(lubridate)

# updateFuelData -----------------------------------------------------------------

update_fuel_data <- function(location, n){
  CurrentData <- read.csv(location) %>% 
    mutate(Month = as.Date(Month, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))) %>% 
    select(Month, Price)
  
  FuelLink = paste("https://www.indexmundi.com/commodities/?commodity=jet-fuel&months=", as.character(n), sep = "")
  webpage <- rvest::read_html(FuelLink)
  UpdateData <- webpage  %>% html_node('#gvPrices') %>%  # Replace "table" with the specific CSS selector if necessary
    html_table() %>% 
    mutate(Month =  as.Date(paste(Month, " 01"), format = "%b %Y %d")) %>%
    mutate(Price = Price / 0.0238095238 ) %>%
    select(Month, Price)
  
  CurrentData <- CurrentData %>% filter(!(Month %in% UpdateData$Month))
  NewData <- rbind(CurrentData,UpdateData )
  write.csv(NewData, location, row.names = FALSE)
}

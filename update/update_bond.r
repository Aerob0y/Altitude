

library(rvest)
library(httr)
# Function -----------------------------------------------------------------


update_bond_data <- function(only_rds = FALSE) {
  if (!only_rds) {
    if (!dir.exists("data/Bond")) {
      data <- download_if_updated_etag("https://www.tenancy.govt.nz/assets/Uploads/Tenancy/Rental-bond-data/Detailed-Monthly-TLA-Tenancy-v2.csv", "data/Bond/Bond.csv")
    }
  }
  if (file.exists("data/Bond/Bond.csv")) {
    data <- read.csv("data/Bond/Bond.csv")
    data$Location[data$Location.Id == "-99"] <- "New Zealand"
    data <- data %>%
      rename(
        Date = `Time.Frame`,
        `Lodged Bonds` = `Lodged.Bonds`,
        `Active Bonds` = `Active.Bonds`,
        `Closed Bonds` = `Closed.Bonds`,
        `Median Rent` = `Median.Rent`,
        `Geometric Mean Rent` = `Geometric.Mean.Rent`,
        `Upper Quartile Rent` = `Upper.Quartile.Rent`,
        `Lower Quartile Rent` = `Lower.Quartile.Rent`,
        `Log Std Dev Weekly Rent` = `Log.Std.Dev.Weekly.Rent`
      ) %>%
      mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
      mutate(`Lodged Bonds` = as.numeric(`Lodged Bonds`)) %>%
      mutate(`Active Bonds` = as.numeric(`Active Bonds`)) %>%
      mutate(`Closed Bonds` = as.numeric(`Closed Bonds`)) %>%
      mutate(`Median Rent` = as.numeric(`Median Rent`)) %>%
      mutate(`Geometric Mean Rent` = as.numeric(`Geometric Mean Rent`)) %>%
      mutate(`Upper Quartile Rent` = as.numeric(`Upper Quartile Rent`)) %>%
      mutate(`Lower Quartile Rent` = as.numeric(`Lower Quartile Rent`)) %>%
      mutate(`Log Std Dev Weekly Rent` = as.numeric(`Log Std Dev Weekly Rent`)) %>%
      arrange(Date)
    saveRDS(data, file = "data/Bond/bond.rds", compress = FALSE)
  }
}

update_bond_data(only_rds = TRUE)


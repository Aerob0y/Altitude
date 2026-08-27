
# ==============================================================================
# README -----------------------------------------------------------------------
# Purpose: Download and process tenancy bond CSV files from Tenancy.govt.nz
# get_tenancy_url() finds the latest CSV file, and update_bond_data() downloads it
# and saves it as a .rds file for use in the app.

get_tenancy_url <- function() {
  base_url <- paste0(
    "https://www.tenancy.govt.nz/assets/Uploads/Tenancy/",
    "Rental-bond-data/detailed-monthly-tla-tenancy-"
  )

  # Try current month, then previous month
  months <- tolower(format(
    seq(Sys.Date(), by = "-1 month", length.out = 2),
    "%B"
  ))

  urls <- paste0(base_url, months, ".csv")

  for (url in urls) {
    response <- httr::HEAD(url)

    if (httr::status_code(response) == 200) {
      return(url)
    }
  }

  stop("Could not find the latest tenancy bond CSV.")
}

update_bond_data <- function(only_rds = FALSE) {
  if (!only_rds) {
    if (dir.exists("app/data/Bond")) {
      url <- get_tenancy_url()
      data <- download_if_updated_etag(url, "app/data/Bond/Bond.csv")
    }
  }
  if (file.exists("app/data/Bond/Bond.csv")) {
    data <- read.csv("app/data/Bond/Bond.csv")
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
      mutate(
        Date = as.Date(Date),
        across(
          c(
            `Lodged Bonds`,
            `Active Bonds`,
            `Closed Bonds`,
            `Median Rent`,
            `Geometric Mean Rent`,
            `Upper Quartile Rent`,
            `Lower Quartile Rent`,
            `Log Std Dev Weekly Rent`
          ),
          ~ readr::parse_number(as.character(.x), na = c("", "NA", "N/A", "-"))
        )
      ) %>%
      arrange(Date)
    saveRDS(data, file = "app/data/Bond/bond.rds", compress = FALSE)
  }
}

update_bond_data()

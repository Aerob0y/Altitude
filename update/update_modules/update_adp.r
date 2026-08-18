# ==============================================================================
# README -----------------------------------------------------------------------
# Purpose: ----------------
# Download and process Accommodation Data Programme (ADP) JSON files.
# Downloads selected ADP JSON files,
# saves dated local copies, and converts the data into an RDS file
# for use elsewhere in the project.

#
# Main functions----------------
# rds_adp_data() converts the downloaded JSON file into an RDS file.
# update_adp_data() downloads the JSON file and then calls rds_adp_data()

rds_adp_data <- function() {
  # Read the csv of converted regions to map the original region names to the new region names
  converted_regions <- here::here("app/data/reference/adp_regions.csv") %>% read.csv2(stringsAsFactors = FALSE, check.names = FALSE,sep = ",") %>%
    select(Original, region_new) %>%
    distinct() %>%
    filter(!is.na(region_new))

  # Read the JSON file and extract the data
  open_json <- fromJSON("app/data/ADP/adpByRTO.json", simplifyVector = FALSE)
  has_data <- purrr::map_lgl(open_json, ~ !is.null(.x$data))
  if (!any(has_data)) stop("No 'data' node found in the JSON.")
  holder <- open_json[[which(has_data)[1]]]$data

  # Convert the nested list into a data frame
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

  # Pivot the data frame to have separate columns for each measure and arrange the rows
  new_data <- out %>%
    pivot_wider(names_from = Measure, values_from = Value) %>%
    arrange(Date, Region, PropertyType)

  # Read the historic CSV file, convert the date column, and ensure numeric columns are properly typed
  historic <- read.csv("app/data/ADP/adp_historic.csv", check.names = FALSE)
  historic$Date <- as.Date(historic$Date, format="%d/%m/%Y")
  historic <- historic %>%
    mutate(
      across(
        c(
          `Number of establishments`,
          `Daily Capacity (stay-units available)`,
          `Available monthly stay unit capacity`,
          `Occupancy rate (%)`,
          `Total guest nights`,
          `Guest arrivals`,
          `Stay unit nights occupied`,
          `Domestic guest nights`,
          `International guest nights`
        ),
        ~ as.numeric(na_if(trimws(.x), ""))
      )
    )
  combined_data <- bind_rows(historic, new_data) %>% distinct()

  # Join the combined data with the converted regions to map the original region names to the new region names, and save the final data frame as an RDS file
  combined_data <- combined_data %>%
    left_join(converted_regions, by = c("Region" = "Original")) %>%
    select(Date, region_new, PropertyType, everything(), -Region) %>%
    rename("Regions" = "region_new")

  combined_data$PropertyType[combined_data$PropertyType == "All Property Types"] <- "All"
  combined_data %>% saveRDS("app/data/ADP/adpByRTO.rds")
}

update_adp_data <- function() {
  download_if_updated_etag("https://teic.mbie.govt.nz/ste/data/views/theEconomy/economicResilience/adpByRTO.json", "app/data/ADP/adpByRTO.json")
  rds_adp_data()
}

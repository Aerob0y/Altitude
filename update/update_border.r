library(readr)
library(dplyr)
library(purrr)

update_border <- function(input_dir = "data/AIAL/",
                               output_rds = "data/AIAL/border.rds",
                               pattern = "\\.csv$") {
  # 1. List CSV files
  files <- list.files(
    path       = input_dir,
    pattern    = pattern,
    full.names = TRUE
  )

  if (length(files) == 0L) {
    stop("No CSV files found in: ", input_dir)
  }

  message("Found ", length(files), " CSV file(s).")

  # 2. Read and combine
  df <- files %>%
    set_names(basename(.)) %>%          # keep file name for reference if you want
    map_dfr(
      ~ read_csv(.x, show_col_types = FALSE, col_types = cols(.default = "c")),
      .id = "source_file"               # adds a column with the file name
    )
  df <- df %>%
    select(-c(DayOfTravel, AgeAtTravelRange, sex_code, occupation_code, ta_code))

  country_mappings <- load_data("airports")
  df$TotalPassengerMovements <- as.numeric(df$TotalPassengerMovements)
  df <- df %>%
    left_join(
      rename(select(country_mappings, place, code), NZPort = place),
      join_by(`customs_port_code` == code)
    )
  df <- df %>%
    left_join(
      rename(select(country_mappings, place, code), OSPort = place),
      join_by(`OverSeasPort` == code)
    )
  df <- df %>%
    left_join(
      rename(select(country_mappings, place, code), OSPort = place),
      join_by(`OverSeasPort` == code)
    )
  df$customs_port_code <- case_when(
    df$customs_port_code == "AKL" ~ "AKL",
    df$customs_port_code == "CHC" ~ "CHC",
    df$customs_port_code == "WLG" ~ "WLG",
    df$customs_port_code == "ZQN" ~ "ZQN",
    TRUE ~ "Other"
  )
  df <- df %>%
    mutate(`Residency/Country` = ifelse(passenger_type_code == "R", main_country_visited_code, clnpr_code)) %>%
    group_by(MonthOfTravel, NZPort, `Residency/Country`, OverSeasPort, customs_port_code, passenger_type_code, travel_purpose_code) %>%
    summarise(TotalPassengers = sum(TotalPassengerMovements), .groups = "drop", quiet = TRUE) %>%
    rename(
      Date = MonthOfTravel,
      `Overseas Port` = OverSeasPort,
      `New Zealand Port` = customs_port_code,
      `Passenger Type` = passenger_type_code,
      `Travel Purpose` = travel_purpose_code
    ) %>%
    mutate(Date = as.Date(paste0(Date, "01"), format("%Y%m%d")))

  # 3. Save as RDS
  saveRDS(df, file = output_rds)
  message("Combined data saved to: ", output_rds)

  invisible(df)
}
update_border()
load_data("border", refresh_cache = TRUE) %>% View()

read.csv("data/Reference/airports.csv") %>% write_rds("data/Reference/airports.rds")
load_data("airports", refresh_cache = TRUE) %>% View()

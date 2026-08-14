
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(fs)
library(digest)

#### Lookups ----
port_lookup_input <- list(
  "Adelaide" = "ADL",
  "Apia" = "APW",
  "Bandar Seri Begawan" = "BWN",
  "Bangkok" = "BKK",
  "Beijing" = "PEK",
  "Brisbane" = "BNE",
  "Buenos Aires" = "EZE",
  "Cairns" = "CNS",
  "Coolangatta" = "OOL",
  "Denpasar" = "DPS",
  "Guangzhou" = "CAN",
  "Hong Kong" = "HKG",
  "Honolulu" = "HNL",
  "Kuala Lumpur" = "KUL",
  "Los Angeles" = "LAX",
  "Melbourne" = "MEL",
  "Nadi" = "NAN",
  "Nagoya" = "NGO",
  "Niue" = "IUE",
  "Norfolk Island" = "NLK",
  "Noumea" = "NOU",
  "Osaka" = "OSA",
  "Papeete" = "PPT",
  "Perth" = "PER",
  "Port Vila" = "VLI",
  "Rarotonga" = "RAR",
  "San Francisco" = "SFO",
  "Santiago" = "SCL",
  "Seoul" = "ICN",
  "Shanghai" = "PVG",
  "Singapore" = "SIN",
  "Suva" = "SUV",
  "Sydney" = "SYD",
  "Taipei" = "TPE",
  "Tokyo" = "NRT",
  "Tonga" = "TBU",
  "Vancouver" = "YVR"
)
port_lookup_input <- tibble(
  name = names(port_lookup_input),
  code = unlist(port_lookup_input)
)
destination_lookup_input <- list(
"Australia" = "AU",
"Cook Islands" = "CK",
"Fiji" = "FJ",
"Micronesia, Federated States of" = "FM",
"Kiribati" = "KI",
"Nauru" = "NR",
"French Polynesia" = "PF",
"Papua New Guinea" = "PG",
"Solomon Islands" = "SB",
"Tokelau" = "TK",
"Tonga" = "TO",
"Vanuatu" = "VU",
"Samoa" = "WS",
"China, People's Republic of" = "CN",
"Hong Kong (Special Administrative Region)" = "HK",
"Indonesia" = "ID",
"India" = "IN",
"Japan" = "JP",
"Cambodia" = "KH",
"Korea, Republic of" = "KR",
"Malaysia" = "MY",
"Sri Lanka" = "LK",
"Nepal" = "NP",
"Philippines" = "PH",
"Pakistan" = "PK",
"Singapore" = "SG",
"Thailand" = "TH",
"Taiwan" = "TW",
"Viet Nam" = "VN",
"Austria" = "AT",
"Belgium" = "BE",
"Switzerland" = "CH",
"Germany" = "DE",
"Denmark" = "DK",
"Spain" = "ES",
"France" = "FR",
"United Kingdom" = "GB",
"Ireland" = "IE",
"Italy" = "IT",
"Malta" = "MT",
"Netherlands" = "NL",
"Poland" = "PL",
"Sweden" = "SE",
"Argentina" = "AR",
"Brazil" = "BR",
"Canada" = "CA",
"Chile" = "CL",
"Colombia" = "CO",
"Ecuador" = "EC",
"Mexico" = "MX",
"United States of America" = "US",
"United Arab Emirates" = "AE",
"Algeria" = "DZ",
"Egypt" = "EG",
"Israel" = "IL",
"Iran" = "IR",
"Jordan" = "JO",
"Oman" = "OM",
"Saudi Arabia" = "SA",
"Turkiye" = "TR",
"South Africa" = "ZA",
"NOT STATED" = "NS",
"New Caledonia" = "NC",
"Norfolk Island" = "NF",
"Bangladesh" = "BD",
"Finland" = "FI",
"Hungary" = "HU",
"Russia" = "RU",
"Costa Rica" = "CR",
"Peru" = "PE",
"Uruguay" = "UY",
"Bahrain" = "BH",
"Botswana" = "BW",
"Kenya" = "KE",
"Malawi" = "MW",
"Macau (Special Administrative Region)" = "MO",
"Czechia" = "CZ",
"Estonia" = "EE",
"North Macedonia" = "MK",
"Norway" = "NO",
"Barbados" = "BB",
"Cuba" = "CU",
"Marshall Islands" = "MH",
"Maldives" = "MV",
"Greece" = "GR",
"Portugal" = "PT",
"Ukraine" = "UA",
"Iraq" = "IQ",
"Tuvalu" = "TV",
"Timor-Leste" = "TL",
"Cyprus" = "CY",
"Croatia" = "HR",
"Libya" = "LY",
"Qatar" = "QA",
"Tanzania" = "TZ",
"Kuwait" = "KW",
"Bulgaria" = "BG",
"Bermuda" = "BM",
"Sierra Leone" = "SL",
"Tunisia" = "TN",
"Kosovo" = "XK",
"Serbia" = "RS",
"Ghana" = "GH",
"Namibia" = "NA",
"Afghanistan" = "AF",
"Iceland" = "IS",
"Romania" = "RO",
"Slovakia" = "SK",
"Zimbabwe" = "ZW",
"Niue" = "NU",
"Latvia" = "LV",
"Belize" = "BZ",
"Morocco" = "MA",
"Zambia" = "ZM",
"Ethiopia" = "ET",
"Mauritius" = "MU",
"Gaza Strip/Palestine/West Bank" = "PS",
"Bahamas" = "BS",
"Grenada" = "GD",
"Samoa, American" = "AS",
"Azerbaijan" = "AZ",
"St Vincent and the Grenadines" = "VC",
"Angola" = "AO",
"Liberia" = "LR",
"Chad" = "TD",
"Cayman Islands" = "KY",
"Laos" = "LA",
"Guam" = "GU",
"Sudan" = "SD",
"Uganda" = "UG",
"Antarctica" = "AQ",
"Christmas Island" = "CX",
"Northern Mariana Islands" = "MP",
"Palau" = "PW",
"Armenia" = "AM",
"Brunei Darussalam" = "BN",
"Bhutan" = "BT",
"Georgia" = "GE",
"Kyrgyzstan" = "KG",
"Korea, Democratic People's Republic of" = "KP",
"Kazakhstan" = "KZ",
"Burma (Myanmar)" = "MM",
"Mongolia" = "MN",
"Turkmenistan" = "TM",
"Uzbekistan" = "UZ",
"Albania" = "AL",
"Bosnia and Herzegovina" = "BA",
"Belarus" = "BY",
"Faeroe Islands" = "FO",
"Gibraltar" = "GI",
"Greenland" = "GL",
"Liechtenstein" = "LI",
"Lithuania" = "LT",
"Luxembourg" = "LU",
"Monaco" = "MC",
"Moldova" = "MD",
"Montenegro" = "ME",
"Slovenia" = "SI",
"Antigua and Barbuda" = "AG",
"Anguilla" = "AI",
"Aruba" = "AW",
"Bolivia" = "BO",
"Curacao" = "CW",
"Falkland Islands" = "FK",
"South Georgia and the South Sandwich Islands" = "GS",
"Guatemala" = "GT",
"Guyana" = "GY",
"Honduras" = "HN",
"Jamaica" = "JM",
"St Kitts and Nevis" = "KN",
"St Lucia" = "LC",
"Montserrat" = "MS",
"Nicaragua" = "NI",
"Panama" = "PA",
"Puerto Rico" = "PR",
"Paraguay" = "PY",
"El Salvador" = "SV",
"Turks and Caicos Islands" = "TC",
"Trinidad and Tobago" = "TT",
"Venezuela" = "VE",
"Virgin Islands, British" = "VG",
"Burkina Faso" = "BF",
"Burundi" = "BI",
"Benin" = "BJ",
"Congo, the Democratic Republic of the" = "CD",
"Congo" = "CG",
"Cote d'Ivoire" = "CI",
"Djibouti" = "DJ",
"Eritrea" = "ER",
"Gabon" = "GA",
"Gambia" = "GM",
"Guinea" = "GN",
"British Indian Ocean Territory" = "IO",
"Lebanon" = "LB",
"Madagascar" = "MG",
"Mali" = "ML",
"Mozambique" = "MZ",
"Nigeria" = "NG",
"Reunion" = "RE",
"Rwanda" = "RW",
"Seychelles" = "SC",
"Senegal" = "SN",
"Somalia" = "SO",
"Syria" = "SY",
"Eswatini" = "SZ",
"Togo" = "TG",
"Yemen" = "YE",
"Cocos (Keeling) Islands" = "CC",
"Pitcairn" = "PN",
"Wallis and Futuna" = "WF",
"Tajikistan" = "TJ",
"Andorra" = "AD",
"Dominica" = "DM",
"Dominican Republic" = "DO",
"Haiti" = "HT",
"Cameroon" = "CM",
"Cabo Verde" = "CV",
"Equatorial Guinea" = "GQ",
"Comoros" = "KM",
"Mauritania" = "MR",
"Niger" = "NE",
"South Sudan" = "SS",
"Guadeloupe" = "GP",
"Martinique" = "MQ",
"Suriname" = "SR",
"St Maarten (Dutch Part)" = "SX",
"St Helena" = "SH",
"United States Minor Outlying Islands" = "UM",
"Lesotho" = "LS",
"Virgin Islands, United States" = "VI",
"French Guiana" = "GF",
"Western Sahara" = "EH",
"Central African Republic" = "CF",
"Netherlands Antilles" = "AN",
"Vatican City State" = "VA",
"Guinea-Bissau" = "GW",
"Sao Tome and Principe" = "ST",
"San Marino" = "SM",
"St Pierre and Miquelon" = "PM",
"Mayotte" = "YT",
"French Southern Territories" = "TF"
)

destination_lookup_input <- tibble(
  Destination = names(destination_lookup_input),
  code = unlist(destination_lookup_input)
)
#### ----

#-----------------------------
# 1) Current StatsNZ builder
#-----------------------------
build_border_current <- function(
    input_dir = "C:/Users/MichaelHawley/OneDrive - Queenstown Airport Corporation/Aeronautical/Dataset/StatsNZ/",
    pattern = "\\.csv$",
    country_mappings = load_data("airports")
) {
  files <- list.files(
    path       = input_dir,
    pattern    = pattern,
    recursive  = TRUE,
    full.names = TRUE
  )

  if (length(files) == 0L) stop("No CSV files found in: ", input_dir)

  df <- files %>%
    set_names(basename(.)) %>%
    map_dfr(
      ~ read_csv(.x, show_col_types = FALSE, col_types = cols(.default = "c")),
      .id = "source_file"
    ) %>%
    select(-any_of(c("DayOfTravel", "AgeAtTravelRange", "sex_code", "occupation_code", "ta_code")))

  df$TotalPassengerMovements <- as.numeric(df$TotalPassengerMovements)

  # Map NZ & overseas port codes to names if desired (keeping your approach)
  df <- df %>%
    left_join(
      rename(select(country_mappings, place, code), NZPort = place),
      by = c("customs_port_code" = "code")
    ) %>%
    left_join(
      rename(select(country_mappings, place, code), OSPort = place),
      by = c("OverSeasPort" = "code")
    )

  # Collapse NZ ports to AKL/CHC/WLG/ZQN/Other (as per your current logic)
  df <- df %>%
    mutate(
      customs_port_code = case_when(
        customs_port_code %in% c("AKL","CHC","WLG","ZQN") ~ customs_port_code,
        TRUE ~ "Other"
      )
    )

  passenger_type_map <- tibble(
    `Passenger Type` = c("R","V","P"),
    `Passenger Type Desc` = c("Resident", "Visitor", "Permanent Migration")
  )

  travel_purpose_map <- tibble(
    `Travel Purpose` = c("B","C","E","H","U","O","V"),
    `Travel Purpose Desc` = c(
      "Business",
      "Conventions/Conferences",
      "Education",
      "Holiday/Vacation",
      "Unspecified/Not Collected",
      "Other",
      "Visit Friends/Relatives"
    )
  )

  out <- df %>%
    mutate(`Residency/Country` = if_else(passenger_type_code == "R", main_country_visited_code, clnpr_code)) %>%
    group_by(MonthOfTravel, `Residency/Country`, OverSeasPort, customs_port_code, passenger_type_code, travel_purpose_code) %>%
    summarise(Arrivals = sum(TotalPassengerMovements, na.rm = TRUE), .groups = "drop") %>%
    transmute(
      Date = as.Date(paste0(str_remove_all(MonthOfTravel, "[^0-9]"), "01"), format = "%Y%m%d"),
      `Residency/Country`,
      `Overseas Port` = OverSeasPort,
      `New Zealand Port` = customs_port_code,
      `Passenger Type` = passenger_type_code,
      `Travel Purpose` = travel_purpose_code,
      Arrivals
    ) %>%
    inner_join(passenger_type_map, by = "Passenger Type") %>%
    mutate(`Passenger Type` = `Passenger Type Desc`) %>%
    select(-`Passenger Type Desc`) %>%
    inner_join(travel_purpose_map, by = "Travel Purpose") %>%
    mutate(`Travel Purpose` = `Travel Purpose Desc`) %>%
    select(-`Travel Purpose Desc`) %>%
    mutate(Version = "Current")

  out
}

#-----------------------------
# 2) Historic builder
#-----------------------------
build_border_historic <- function(
  nz_closest_overseas_csv = here(
    "app",
    "data",
    "Border Historic",
    "NZ-resident traveller arrivals by closest overseas port (Monthly)",
    "NZ-resident traveller arrivals by closest overseas port (Monthly).csv"
  ),
  visitor_closest_overseas_csv = here(
    "app",
    "data",
    "Border Historic",
    "Visitor arrivals by country of residence and closest overseas port (Monthly)",
    "Visitor arrivals by country of residence and closest overseas port (Monthly).csv"
  ),
  nz_destination_dir = here(
    "app",
    "data",
    "Border Historic",
    "NZ-resident traveller arrivals by EVERY country of main destination and purpose (Monthly)"
  ),
  visitor_residence_dir = here(
    "app",
    "data",
    "Border Historic",
    "Visitor arrivals by EVERY country of residence and purpose (Monthly)"
  ),
  port_lookup = port_lookup_input,
  destination_lookup = destination_lookup_input
) {
  # Resident arrivals by closest overseas port
  nz_close <- read_csv(nz_closest_overseas_csv, skip = 1, show_col_types = FALSE) %>%
    pivot_longer(cols = -1, names_to = "Overseas Port", values_to = "Arrivals") %>%
    mutate(Date = as.Date(paste0(str_replace(Date, "M", "-"), "-01"))) %>%
    filter(Arrivals > 0) %>%
    filter(Date < as.Date("2018-11-01", format = "%Y-%m-%d")) %>% # pre-StatsNZ data to avoid overlap
    inner_join(port_lookup, by = c("Overseas Port" = "name")) %>%
    transmute(
      Date,
      `Residency/Country` = "Total",
      `Overseas Port` = code,
      `New Zealand Port` = "Total",
      `Passenger Type` = "Resident",
      `Travel Purpose` = "Total",
      Arrivals
    ) %>%
    mutate(Version = "Port")
    head(nz_close)


  # Visitor arrivals by closest overseas port
  os_close <- read_csv(visitor_closest_overseas_csv, skip = 2, show_col_types = FALSE) %>%
    pivot_longer(cols = -1, names_to = "Overseas Port", values_to = "Arrivals") %>%
    mutate(Date = as.Date(paste0(str_replace(Date, "M", "-"), "-01"))) %>%
    filter(Arrivals > 0) %>%
    filter(Date < as.Date("2018-11-01", format = "%Y-%m-%d")) %>% # pre-StatsNZ data to avoid overlap
    inner_join(port_lookup, by = c("Overseas Port" = "name")) %>%
    transmute(
      Date,
      `Residency/Country` = "Total",
      `Overseas Port` = code,
      `New Zealand Port` = "Total",
      `Passenger Type` = "Visitor",
      `Travel Purpose` = "Total",
      Arrivals
    ) %>%
    mutate(Version = "Port")
    head(os_close)

  # Residents by destination + purpose (many files)
  nz_destination <- list.files(nz_destination_dir, pattern = "\\.csv$", full.names = TRUE) %>%
    map_dfr(~ read_csv(.x, skip = 2,
                      col_names = c(
                        "Date","Destination","Business","Conventions/Conferences","Education",
                        "Holiday/Vacation","Unspecified/Not Collected","Other","Visit Friends/Relatives",
                        "TOTAL ALL TRAVEL PURPOSES"
                      ),
                      show_col_types = FALSE,
                      col_types = cols(.default = "c")
    )) %>%
    fill(Date) %>%
    mutate(Date = as.Date(paste0(str_replace(Date, "M", "-"), "-01"))) %>%
    filter(Destination != "") %>%
    filter(Date < as.Date("2018-11-01", format = "%Y-%m-%d")) %>% # pre-StatsNZ data to avoid overlap
    pivot_longer(cols = -c(Date, Destination), names_to = "Purpose", values_to = "Arrivals") %>%
    mutate(Arrivals = suppressWarnings(as.numeric(str_replace_all(Arrivals, "\\.\\.", "0")))) %>%
    filter(!Destination %in% c("TOTAL ALL COUNTRIES OF MAIN DESTINATION","OCEANIA","ASIA","EUROPE","AMERICAS","AFRICA AND THE MIDDLE EAST")) %>%
    filter(Purpose != "TOTAL ALL TRAVEL PURPOSES") %>%
    filter(Arrivals > 0) %>%
    left_join(destination_lookup, by = c("Destination" = "Destination")) %>%
    transmute(
      Date,
      `Residency/Country` = code,
      `Overseas Port` = "Total",
      `New Zealand Port` = "Total",
      `Passenger Type` = "Resident",
      `Travel Purpose` = Purpose,
      Arrivals
    ) %>%
    mutate(Version = "Destination")
    head(nz_destination)

  # Visitors by residence + purpose (many files)
  os_residence <- list.files(visitor_residence_dir, pattern = "\\.csv$", full.names = TRUE) %>%
    map_dfr(~ read_csv(.x, skip = 2,
                      col_names = c(
                        "Date","Destination","Business","Conventions/Conferences","Education",
                        "Holiday/Vacation","Unspecified/Not Collected","Other","Visit Friends/Relatives",
                        "TOTAL ALL TRAVEL PURPOSES"
                      ),
                      show_col_types = FALSE,
                      col_types = cols(.default = "c")
    )) %>%
    fill(Date) %>%
    mutate(Date = as.Date(paste0(str_replace(Date, "M", "-"), "-01"))) %>%
    filter(Date < as.Date("2018-11-01", format = "%Y-%m-%d")) %>% # pre-StatsNZ data to avoid overlap
    filter(!Destination %in% c("TOTAL ALL COUNTRIES OF RESIDENCE","OCEANIA","ASIA","EUROPE","AMERICAS","AFRICA AND THE MIDDLE EAST")) %>%
    pivot_longer(cols = -c(Date, Destination), names_to = "Purpose", values_to = "Arrivals") %>%
    mutate(Arrivals = suppressWarnings(as.numeric(Arrivals))) %>%
    filter(Purpose != "TOTAL ALL TRAVEL PURPOSES") %>%
    filter(Arrivals > 0) %>%
    left_join(destination_lookup, by = c("Destination" = "Destination")) %>%
    transmute(
      Date,
      `Residency/Country` = code,
      `Overseas Port` = "Total",
      `New Zealand Port` = "Total",
      `Passenger Type` = "Visitor",
      `Travel Purpose` = Purpose,
      Arrivals
    ) %>%
    mutate(Version = "Destination")
  head(os_residence)

  bind_rows(nz_close, os_close, nz_destination, os_residence) %>%
    mutate(
      Date = as.Date(Date),
      Arrivals = as.numeric(Arrivals)
    )
}

#-----------------------------
# 3) Orchestrator with change detection
#-----------------------------




historic_inputs_input <- list(
  nz_closest_overseas_csv = here(
    "app",
    "data",
    "Border Historic",
    "NZ-resident traveller arrivals by closest overseas port (Monthly)",
    "NZ-resident traveller arrivals by closest overseas port (Monthly).csv"
  ),
  visitor_closest_overseas_csv = here(
    "app",
    "data",
    "Border Historic",
    "Visitor arrivals by country of residence and closest overseas port (Monthly)",
    "Visitor arrivals by country of residence and closest overseas port (Monthly).csv"
  ),
  nz_destination_dir = here(
    "app",
    "data",
    "Border Historic",
    "NZ-resident traveller arrivals by EVERY country of main destination and purpose (Monthly)"
  ),
  visitor_residence_dir = here(
    "app",
    "data",
    "Border Historic",
    "Visitor arrivals by EVERY country of residence and purpose (Monthly)"
  ),
  port_lookup = port_lookup_input,
  destination_lookup = destination_lookup_input
)

build_border_historic()

update_border_all <- function(
    current_input_dir = "C:/Users/MichaelHawley/OneDrive - Queenstown Airport Corporation/Aeronautical/Dataset/StatsNZ/",
    country_mappings = load_data("airports"),
    historic_inputs = historic_inputs_input,
    output_dir = "app/data/Border",
    pattern = "\\.csv$"
) {
  dir_create(output_dir)

  out_current <- file.path(output_dir, "border_current.rds")
  out_hist    <- file.path(output_dir, "border_historic.rds")
  out_all     <- file.path(output_dir, "border_all.rds")
  sig_file    <- file.path(output_dir, "border_all.sig.rds")

  # signature across BOTH current folder + historic files/dirs
  sig_paths <- c(
    list.files(current_input_dir, pattern = pattern, recursive = TRUE, full.names = TRUE),
    historic_inputs$nz_closest_overseas_csv,
    historic_inputs$visitor_closest_overseas_csv,
    list.files(historic_inputs$nz_destination_dir, pattern = "\\.csv$", full.names = TRUE),
    list.files(historic_inputs$visitor_residence_dir, pattern = "\\.csv$", full.names = TRUE)
  ) %>% unique()

  info <- file.info(sig_paths)
  sig_tbl <- tibble(
    path  = normalizePath(sig_paths, winslash = "/", mustWork = FALSE),
    size  = info$size,
    mtime = as.numeric(info$mtime)
  ) %>% arrange(path)

  current_sig <- digest::digest(sig_tbl, algo = "xxhash64")

  if (file.exists(out_all) && file.exists(sig_file)) {
    old_sig <- readRDS(sig_file)
    if (identical(old_sig, current_sig)) {
      message("No changes detected. Skipping update.")
      return(invisible(readRDS(out_all)))
    }
  }

  # build
  current <- build_border_current(
    input_dir = current_input_dir,
    pattern = pattern,
    country_mappings = country_mappings
  )

  historic <- build_border_historic(
    nz_closest_overseas_csv = historic_inputs$nz_closest_overseas_csv,
    visitor_closest_overseas_csv = historic_inputs$visitor_closest_overseas_csv,
    nz_destination_dir = historic_inputs$nz_destination_dir,
    visitor_residence_dir = historic_inputs$visitor_residence_dir,
    port_lookup = historic_inputs$port_lookup,
    destination_lookup = historic_inputs$destination_lookup
  )

  all <- bind_rows(historic, current) %>%
    distinct() %>%
    arrange(Date)

  saveRDS(current, out_current)
  saveRDS(historic, out_hist)
  saveRDS(all, out_all)
  saveRDS(current_sig, sig_file)

  invisible(all)
}

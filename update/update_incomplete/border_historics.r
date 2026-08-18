library(dplyr)
library(here)

port_looukup <- list(
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
port_looukup <- tibble(
  name = names(port_looukup),
  code = unlist(port_looukup)
)
destination_lookup <- list(
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
destination_lookup <- tibble(
  Destination = names(destination_lookup),
  code = unlist(destination_lookup)
)



nz_close <- read_csv(
  here(
    "app",
    "data",
    "Border Historic",
    "NZ-resident traveller arrivals by closest overseas port (Monthly)",
    "NZ-resident traveller arrivals by closest overseas port (Monthly).csv"
  ),
  skip = 1,
  show_col_types = FALSE
) %>%
  pivot_longer(cols = -c(1), names_to = "Overseas Port", values_to = "Arrivals") %>%
  mutate(Date = paste0(str_replace(Date, "M", "-"), "-01")) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  filter(Arrivals > 0) %>% 
  inner_join(port_looukup, by = c("Overseas Port" = "name")) %>%
  select(-`Overseas Port`) %>%
  rename(Overseas_Port = code) %>%
  mutate("Type" = "NZ Resident")

os_close <- read_csv(
  here(
    "app",
    "data",
    "Border Historic",
    "Visitor arrivals by country of residence and closest overseas port (Monthly)",
    "Visitor arrivals by country of residence and closest overseas port (Monthly).csv"
  ),
  skip = 2,
  show_col_types = FALSE
) %>%
  pivot_longer(cols = -c(1), names_to = "Overseas Port", values_to = "Arrivals") %>%
  mutate(Date = paste0(str_replace(Date, "M", "-"), "-01")) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  filter(Arrivals > 0)%>%
  inner_join(port_looukup, by = c("Overseas Port" = "name")) %>%
  select(-`Overseas Port`) %>%
  rename(Overseas_Port = code) %>%
  mutate("Type" = "OS Resident")

nz_destination <- list.files(
  path       = here(
    "app",
    "data",
    "Border Historic",
    "NZ-resident traveller arrivals by EVERY country of main destination and purpose (Monthly)"
  ),
  pattern    = "*.csv",
  recursive = TRUE,
  full.names = TRUE
) %>%
  map_dfr(
    ~ read_csv(.x,  skip = 2, col_names = c("Date", "Destination", "Business", "Conventions/Conferences", "Education", "Holiday/Vacation", "Unspecified/Not Collected", "Other", "Visit Friends/Relatives", "TOTAL ALL TRAVEL PURPOSES")),
    .id = "source_file"               # adds a column with the file name
  ) %>%
  fill(Date) %>% 
  mutate(Date = paste0(str_replace(Date, "M", "-"), "-01")) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  select(-source_file) %>%
  filter(Destination != "") %>%
  mutate_at(c("Business", "Conventions/Conferences", "Education", "Holiday/Vacation", "Unspecified/Not Collected", "Other", "Visit Friends/Relatives", "TOTAL ALL TRAVEL PURPOSES"), ~str_replace(., "..", "0")) %>%
  mutate_at(c("Business", "Conventions/Conferences", "Education", "Holiday/Vacation", "Unspecified/Not Collected", "Other", "Visit Friends/Relatives", "TOTAL ALL TRAVEL PURPOSES"), as.numeric) %>%
  pivot_longer(cols = -c(1:2), names_to = "Purpose", values_to = "Arrivals") %>%
  filter(Arrivals > 0) %>%
  filter(!Destination %in% c("TOTAL ALL COUNTRIES OF MAIN DESTINATION", "OCEANIA", "ASIA", "EUROPE", "AMERICAS", "AFRICA AND THE MIDDLE EAST")) %>%
  left_join(destination_lookup, by = "Destination") %>%
  select(-Destination) %>%
  rename(Destination = code) %>%
  mutate(Type = "NZ Resident") %>%
  filter(Purpose != "TOTAL ALL TRAVEL PURPOSES")

os_residence <- list.files(
  path       = here(
    "app",
    "data",
    "Border Historic",
    "Visitor arrivals by EVERY country of residence and purpose (Monthly)"
  ),
  pattern    = "\\.csv$",
  recursive  = TRUE,
  full.names = TRUE
) %>%
  set_names(.) %>%                       # so .id becomes the file path
  map_dfr(
    .f = ~ read_csv(
      .x,
      skip = 2,
      col_names = c(
        "Date", "Destination", "Business", "Conventions/Conferences", "Education",
        "Holiday/Vacation", "Unspecified/Not Collected", "Other",
        "Visit Friends/Relatives", "TOTAL ALL TRAVEL PURPOSES"
      ),
      col_types = "cccccccccc",
      show_col_types = FALSE
    ),
    .id = "source_file"
  ) %>%
  fill(Date) %>%
  mutate(Date = paste0(str_replace(Date, "M", "-"), "-01")) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  select(-source_file) %>%
  mutate_at(c("Business", "Conventions/Conferences", "Education", "Holiday/Vacation", "Unspecified/Not Collected", "Other", "Visit Friends/Relatives", "TOTAL ALL TRAVEL PURPOSES"), as.numeric) %>%
  filter(!Destination %in% c("TOTAL ALL COUNTRIES OF MAIN DESTINATION", "OCEANIA", "ASIA", "EUROPE", "AMERICAS", "AFRICA AND THE MIDDLE EAST")) %>%
  pivot_longer(cols = -c(1:2), names_to = "Purpose", values_to = "Arrivals") %>%
  filter(Arrivals > 0) %>%
  left_join(destination_lookup, by = "Destination") %>%
  select(-Destination) %>%
  rename(Destination = code) %>%
  mutate(Type = "OS Resident") %>%
  filter(Purpose != "TOTAL ALL TRAVEL PURPOSES")

nz_close <- nz_close %>%
  mutate("Purpose" = "Total")

os_close <- os_close %>%
  mutate("Purpose" = "Total")

nz_destination <- nz_destination %>%
  mutate(Overseas_Port = "Total")

os_residence <- os_residence %>%
  mutate(Overseas_Port = "Total")

historics <- bind_rows(nz_close, os_close, nz_destination, os_residence) 

historics <- historics %>%
  rename(
    Date = Date,
    `Residency/Country` = Destination,
    `Overseas Port` = Overseas_Port,
    `Travel Purpose` = Purpose,
    `Passenger Type` = Type,
  )

saveRDS(historics, "app/data/border_historics.rds")

load_data("border", refresh_cache = TRUE) %>% view()

x <- load_data("border", refresh_cache = TRUE)
x$`Travel Purpose` %>% unique()
historics$`Travel Purpose` %>% unique()

purpose_mapping <- tibble(
  `Travel Purpose` = c("Business", "Conventions/Conferences", "Education", "Holiday/Vacation", "Unspecified/Not Collected", "Other", "Visit Friends/Relatives"),
  `Code` = c("B", "C", "E", "H", "U", "O", "V")
)

x <- x %>%
  inner_join(purpose_mapping, by = c("Travel Purpose" = "Code"))
  #select(-`Travel Purpose`) %>%
  rename(`Travel Purpose` = Code)

x %>% view()

x$`Passenger Type` %>% unique()

updates(what = "border", manual = TRUE)
